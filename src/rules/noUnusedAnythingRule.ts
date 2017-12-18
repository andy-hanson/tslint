/**
 * @license
 * Copyright 2018 Palantir Technologies, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import {
    getVariableDeclarationKind,
    hasModifier,
    isSymbolFlagSet,
    isTypeFlagSet,
    isTypeReference,
    isUnionOrIntersectionType,
    VariableDeclarationKind
} from "tsutils";
import * as ts from "typescript";

import * as Lint from "..";

import { AnalysisResult, getInfo } from "./analysis";
import { getDeprecationFromDeclaration } from "./analysis/deprecationUtils";
import { hasEnumUse, EnumUse } from "./analysis/enumUse";
import { getPrivacyScope } from "./analysis/privacy";
import { SymbolUses, isNameOfMutableCollectionType } from "./analysis/use";
import { isUsageTrackedDeclaration, UsageTrackedDeclaration } from "./analysis/utils";
import { isNodeFlagSet } from 'tsutils';
import { find } from '../utils';
import { isImplicitlyExported } from './analysis/importUtils';

export class Rule extends Lint.Rules.TypedRule {
    /* tslint:disable:object-literal-sort-keys */
    public static metadata: Lint.IRuleMetadata = {
        ruleName: "no-unused-anything",
        description: Lint.Utils.dedent`
            For every symbol, looks for all uses of that symbol to determine whether it has more capabilities than necessary.

            * Public class members which could be private.
            * Exported symbols that don't need to be exported.
            * Mutable properties that could be \`readonly\`.
            * Mutable variables that could be \`const\`.
            * Collections that are only read from, but are not typed as a \`ReadonlyArray\`, \`ReadonlySet\`, or \`ReadonlyMap\`.
            * Collections that are initialized to a fresh collection (\`[]\`, \`new Set(...)\`, \`new Map(...)\`)
              and have elements inserted, but never read from.
            * Functions that return a value but are only used for a side effect.
            * Optional properties that are read from, but are never assigned to.
            * Enum members that are tested for, but never used as a value.

            Note that looking for all uses of a symbol works best when as many files as possible are included in the tsconfig.
            When linting a library, it is recommended to lint with a \`tsconfig.json\` that includes tests,
            which should use of public exports of the library that are not used internally.

            This rule will be disabled on anything inside a node marked with a \`@deprecated\` jsdoc comment,
            since it is assumed that those will naturally have no uses.
            `,
        optionsDescription: "Not configurable.",
        options: null,
        optionExamples: [true],
        type: "functionality",
        typescriptOnly: false,
    };
    /* tslint:enable:object-literal-sort-keys */

    public applyWithProgram(sourceFile: ts.SourceFile, program: ts.Program): Lint.RuleFailure[] {
        const info = getInfo(program);
        const checker = program.getTypeChecker();
        return this.applyWithWalker(new Walker(sourceFile, this.ruleName, parseOptions(this.getOptions()), info, checker));
    }
}

interface JsonOptions {
    readonly ignoreNames?: ReadonlyArray<string>;
}

function parseOptions(gotOptions: Lint.IOptions): Options {
    const obj0 = gotOptions.ruleArguments[0] as JsonOptions;
    const obj = obj0 === undefined ? {} : obj0;
    return {
        ignoreNames: obj.ignoreNames === undefined ? new Set() : new Set(obj.ignoreNames),
    }
}

interface Options {
    readonly ignoreNames: ReadonlySet<string>;
}

class Walker extends Lint.AbstractWalker<Options> {
    constructor(
        sourceFile: ts.SourceFile,
        ruleName: string,
        options: Options,
        private readonly analysis: AnalysisResult,
        private readonly checker: ts.TypeChecker,
    ) {
        super(sourceFile, ruleName, options);
    }

    public walk(node: ts.Node): void {
        switch (node.kind) {
            case ts.SyntaxKind.Parameter:
                if (ts.isIdentifier((node as ts.ParameterDeclaration).name)) {
                    this.checkParameter(node as NamedParameter);
                }
                break;
            case ts.SyntaxKind.EnumMember:
                this.checkEnumMember(node as ts.EnumMember);
                break;
            default:
                if (isUsageTrackedDeclaration(node)) {
                    this.checkSymbol(node, this.checker.getSymbolAtLocation(node.name)!);
                }
        }
        node.forEachChild(child => this.walk(child));
    }

    private checkParameter(node: NamedParameter): void { //name
        if (ts.isParameterPropertyDeclaration(node)) {
            // Don't care if the parameter side is unused, since it's used to create the property.
            // So, only check the property side.
            const prop = getPropertyOfParameterProperty(node, this.checker);
            if (prop !== undefined) {
                this.checkSymbol(node, prop);
            }
        }
        // If there's no body we can't check whether the parameter is used.
        else if (!!(node.parent! as any).body
            && ts.isIdentifier(node.name)
            && node.name.originalKeywordKind !== ts.SyntaxKind.ThisKeyword) { // Todo: handle 'this'
            this.checkSymbol(node, this.checker.getSymbolAtLocation(node.name)!);
        }
    }

    private checkEnumMember(node: ts.EnumMember): void {
        const flags = this.analysis.getEnumAccessFlags(this.checker.getSymbolAtLocation(node.name)!);
        if (!hasEnumUse(flags, EnumUse.UsedInExpression)) {
            this.addFailureAtNode(node.name,
                hasEnumUse(flags, EnumUse.Tested)
                ? "Enum member is compared against, but analysis found no cases of something being given this value."
                : "Analysis found no uses of this symbol.");
        }
    }

    private checkSymbol(node: UsageTrackedDeclaration, symbol: ts.Symbol): void {
        if (isOkToNotUse(node, symbol, this.checker, this.options.ignoreNames)) {
            return;
        }

        const info = this.analysis.getSymbolUses(symbol);
        this.checkSymbolUses(node, symbol, info);
        if (!info.everUsedAsMutableCollection) {
            this.warnOnNonMutatedCollection(node);
        }
    }

    private checkSymbolUses(node: UsageTrackedDeclaration, symbol: ts.Symbol, uses: SymbolUses): void {
        const fail = (failure: string): void => { this.addFailureAtNode(node.name, failure); };

        if (!uses.everUsedPublicly) {
            const privacyScope = getPrivacyScope(node);
            if (privacyScope === undefined) {
                fail("Analysis found no uses of this symbol.");
            } else if (ts.isSourceFile(privacyScope)) {
                if (!isImplicitlyExported(node, this.sourceFile, this.checker)) {
                    fail("Analysis found no uses in other modules; this should not be exported.");
                }
            } else {
                if (!hasModifier(node.modifiers, ts.SyntaxKind.PrivateKeyword, ts.SyntaxKind.AbstractKeyword)) {
                    fail("Analysis found no public uses; this should be private.");
                }
            }
        } else if (!uses.everRead) {
            //might be a fn used for a side effect.
            if (uses.everUsedForSideEffect && isFunctionLike(symbol)) {
                const sig = this.checker.getSignatureFromDeclaration(symbol.valueDeclaration as ts.SignatureDeclaration)!;
                if (!isTypeFlagSet(this.checker.getReturnTypeOfSignature(sig), ts.TypeFlags.Void)) {
                    fail("This function is used for its side effect, but the return value is never used.");
                }
            } else {
                if (uses.mutatesCollection) {
                    if (!uses.everAssignedANonFreshValue) {
                        fail("This collection is created and filled with values, but never read from.");
                    }
                }
                else {
                    fail("This symbol is given a value, but analysis found no reads. This is likely dead code.");
                }
            }
        } else if (!uses.everWritten && isMutable(node)) {
            fail(ts.isVariableDeclaration(node)
                ? "Analysis found no mutable uses of this variable; use `const`." //tests
                : "Analysis found no writes to this property; mark it as `readonly`.");
        } else if (!uses.everCreatedOrWritten) {
            fail("Analysis found places where this is read, but found no places where it is given a value.");
        }
    }

    private warnOnNonMutatedCollection(node: UsageTrackedDeclaration): void {
        if (ts.isParameter(node) && node.dotDotDotToken !== undefined) {
            // Can't recommend to use `...args: ReadonlyArray<number>`.
            // See https://github.com/Microsoft/TypeScript/issues/15972
            return;
        }

        const typeNode = getTypeAnnotationNode(node);
        const info = typeNode === undefined
            ? shouldTypeReadonlyCollectionWithInferredType(node)
                ? getMutableCollectionTypeFromType(this.checker.getTypeAtLocation(node.name))
                : undefined
            : getMutableCollectionTypeFromNode(typeNode);
        if (info !== undefined) {
            this.addFailureAtNode(
                typeof info === "string" ? node.name : info.typeNode,
                `Analysis indicates this could be a Readonly${typeof info === "string" ? info : info.typeName}.`);
        }
    }
}

// For class property or export, warn if the implicit type is ReadonlyArray.
// But for a local variable, don't demand a type annotation everywhere an array isn't mutated, because that's annoying.
function shouldTypeReadonlyCollectionWithInferredType(node: UsageTrackedDeclaration): boolean {//test
    if (ts.isPropertyDeclaration(node)) {
        return true;
    }
    if (ts.isVariableDeclaration(node)) {
        //helper for this...
        const p = node.parent!;
        if (ts.isVariableDeclarationList(p)) {
            const pp = p.parent!;
            if (ts.isVariableStatement(pp)) {
                return ts.isSourceFile(pp.parent!);
            }
        }
    }
    return false;
}

//returns the type name
function getMutableCollectionTypeFromType(type: ts.Type): string | undefined {
    if (isUnionOrIntersectionType(type)) { //isUnionType helper
        return find(type.types, getMutableCollectionTypeFromType);
    }
    if (isTypeReference(type)) {
        const { symbol } = type.target;
        if (symbol !== undefined && isNameOfMutableCollectionType(symbol.name)) {
            return symbol.name;
        }
    }
    return undefined;
}
//mostly dup of above, break out a utility?
export function isReadonlyType(type: ts.Type): boolean {
    //Allow 'any'
    //todo: use && and || and whatnot
    if (isTypeFlagSet(type, ts.TypeFlags.Any)) {
        return true;
    }
    if (isUnionOrIntersectionType(type)) {
        return type.types.some(isReadonlyType);
    }
    if (isTypeReference(type)) {
        //todo: try just `type.symbol !== undefined && type.symbol.name.startsWith("Readonly")
        const { symbol } = type.target;
        return symbol !== undefined && symbol.name.startsWith("Readonly");
    }
    return false;
}


function getMutableCollectionTypeFromNode(
    typeNode: ts.TypeNode,
): { readonly typeNode: ts.TypeNode, readonly typeName: string } | undefined {
    return ts.isUnionTypeNode(typeNode)
        //Report errors if `string | string[]` is used immutably (should be `string | ReadonlyArray<string>`) (test)
        ? find(typeNode.types, getMutableCollectionTypeFromNode)
        : ts.isArrayTypeNode(typeNode)
        ? { typeNode, typeName: "Array" }
        : ts.isTypeReferenceNode(typeNode)
            && ts.isIdentifier(typeNode.typeName)
            && isNameOfMutableCollectionType(typeNode.typeName.text)
        ? { typeNode, typeName: typeNode.typeName.text }
        : undefined;
}

function getTypeAnnotationNode(node: UsageTrackedDeclaration): ts.TypeNode | undefined {
    switch (node.kind) {
        case ts.SyntaxKind.Parameter:
        //Can't recommend `...args: ReadonlyArray<x>` since that's not allowed by TS
            //return (node as ts.ParameterDeclaration).dotDotDotToken === undefined ? (node as ts.ParameterDeclaration).type : undefined;
        case ts.SyntaxKind.VariableDeclaration:
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.MethodDeclaration:
        case ts.SyntaxKind.MethodSignature:
        case ts.SyntaxKind.FunctionDeclaration:
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.PropertySignature:
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
            type T =
                | ts.ParameterDeclaration | ts.VariableDeclaration | ts.PropertyDeclaration
                | ts.MethodDeclaration | ts.MethodSignature | ts.FunctionDeclaration
                | ts.PropertyDeclaration | ts.PropertySignature | ts.GetAccessorDeclaration | ts.SetAccessorDeclaration;
            return (node as T).type;
        case ts.SyntaxKind.TypeAliasDeclaration:
        case ts.SyntaxKind.EnumDeclaration:
        case ts.SyntaxKind.InterfaceDeclaration:
        case ts.SyntaxKind.ModuleDeclaration:
            return undefined;
        default:
            throw new Error(`TODO: Handle ${ts.SyntaxKind[node.kind]}`);
    }
}


//reuse
function isFunctionLike(symbol: ts.Symbol): boolean {
    return isSymbolFlagSet(symbol, ts.SymbolFlags.Function | ts.SymbolFlags.Method);
}

//note: returns false for things that can't be made immutable (parameters, catch clause variables, methods)
function isMutable(node: UsageTrackedDeclaration): boolean {
    if (ts.isPropertyDeclaration(node) || ts.isPropertySignature(node) || ts.isParameterPropertyDeclaration(node)) {
        return !hasModifier(node.modifiers, ts.SyntaxKind.ReadonlyKeyword);
    } else if (ts.isVariableDeclaration(node)) {
        const parent = node.parent!;
        return parent.kind !== ts.SyntaxKind.CatchClause && getVariableDeclarationKind(parent) !== VariableDeclarationKind.Const;
    } else {
        return false;
    }
}

function isOkToNotUse(
    node: UsageTrackedDeclaration,
    symbol: ts.Symbol,
    checker: ts.TypeChecker,
    ignoreNames: ReadonlySet<string>,
): boolean {
    return isAmbient(node)
        || isIgnored(node)
        || isDeprecated(node)
        || isOverload(node, symbol, checker)
        || isOverride(node, checker)
        || ignoreNames.has(node.name.text);
}

function isAmbient(node: UsageTrackedDeclaration): boolean { //test
    return isNodeFlagSet(node, (ts.NodeFlags as any).Ambient) //todo: make that flag public
}

function isIgnored(node: UsageTrackedDeclaration): boolean {
    return node.name.text.startsWith("_");
}

function isDeprecated(node: ts.Node): boolean {
    return !ts.isSourceFile(node) && (getDeprecationFromDeclaration(node) !== undefined || isDeprecated(node.parent!));
}

function isOverload(node: UsageTrackedDeclaration, symbol: ts.Symbol, checker: ts.TypeChecker): boolean {
    if (!ts.isParameter(node)) {
        return false;
    }
    if (symbol.declarations!.length !== 1) { //can I just delete this? (test first)
        return true;
    }

    const name = ts.getNameOfDeclaration(node.parent!);
    const signatureSymbol = name === undefined ? undefined : checker.getSymbolAtLocation(name);
    return signatureSymbol !== undefined && signatureSymbol.declarations!.length !== 1;
}


function isOverride(node: UsageTrackedDeclaration, checker: ts.TypeChecker): boolean { //test
    const name = node.name.text;
    const parent = node.parent!;
    if (!ts.isClassLike(parent)) {
        return false;
    }
    return parent.heritageClauses !== undefined && parent.heritageClauses.some(clause =>
        clause.types.some(typeNode =>
            checker.getPropertyOfType(checker.getTypeFromTypeNode(typeNode), name) !== undefined));
}

type NamedParameter =  ts.ParameterDeclaration & { readonly name: ts.Identifier };

function getPropertyOfParameterProperty(p: NamedParameter, checker: ts.TypeChecker): ts.Symbol {
    return checker.getSymbolsOfParameterPropertyDeclaration(p, p.name.text).find(s =>
            isSymbolFlagSet(s, ts.SymbolFlags.Property))!;
}
