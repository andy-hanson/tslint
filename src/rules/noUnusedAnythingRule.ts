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

import assert = require("assert");
import { hasModifier, isSymbolFlagSet, getVariableDeclarationKind, VariableDeclarationKind, isSignatureDeclaration } from "tsutils";
import * as ts from "typescript";

import * as Lint from "..";

import { isTested, Tested, AnalysisResult, getInfo, hasEnumAccessFlag, EnumAccessFlags, canBePrivate, isTypeReference } from './analysis';
import { isNodeFlagSet } from 'tsutils';
import { getSymbolDeprecation } from './analysis/moarUtils';
import { AccessFlags, SymbolInfo, isMutableCollectionTypeName, isMutableCollectionTypeNameStr } from './analysis/accessFlags';
import { find } from '../utils';

//todo: warn for unused `export const`
//todo: check type declarations -- e.g. if an array is never pushed to, use a ReadonlyArray
//todo: detect parameter of recursive fn only passed to another instance of the same fn
//todo: detect recursive unused functions (maybe a different rule for that)
//todo: document that `@deprecated` comments disable this rule
//todo: recommend const enum if enum is never indexed

//todo: warn on optional parameters never passed

//todo: separate rule: no-mutate-method

export class Rule extends Lint.Rules.TypedRule {
    /* tslint:disable:object-literal-sort-keys */
    public static metadata: Lint.IRuleMetadata = {
        ruleName: "no-unused-anything",
        description: Lint.Utils.dedent`
            AAAA.`,
        optionsDescription: "Not configurable.",
        options: null,
        optionExamples: [true],
        type: "functionality",
        typescriptOnly: false,
    };
    /* tslint:enable:object-literal-sort-keys */

    public applyWithProgram(sourceFile: ts.SourceFile, program: ts.Program): Lint.RuleFailure[] {
        const errs = ts.getPreEmitDiagnostics(program, sourceFile);
        for (const err of errs) {
            const text = ts.flattenDiagnosticMessageText(err.messageText, "\n");
            const { file, start } = err;
            if (file) {
                const file = err.file!;
                const pos = file.getLineAndCharacterOfPosition(start!);
                console.log(file.fileName, pos, text);
            } else {
                console.log("<no file>", text);
            }
        }
        assert(errs.length === 0); //kill

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
        private readonly info: AnalysisResult,//name
        private readonly checker: ts.TypeChecker,
    ) {
        super(sourceFile, ruleName, options);
    }

    public walk(sourceFile: ts.SourceFile): void {
        const cb = (node: ts.Node) => {
            switch (node.kind) {
                //case ts.SyntaxKind.PropertySignature:
                //case ts.SyntaxKind.PropertyDeclaration:
                //    this.handleProperty(node as ts.PropertyDeclaration | ts.PropertySignature);
                //    break;
                case ts.SyntaxKind.Parameter: {
                    const p = node as ts.ParameterDeclaration & { readonly name: ts.Identifier };
                    if (ts.isParameterPropertyDeclaration(node)) {
                        //Don't care if the parameter-side is unused, since it's used to create the property.
                        this.handleSymbol(p, getThePropertySymbol(p, this.checker));
                    }
                    //TODO: for a signature, look into its implementers for uses
                    else if (!!(p.parent! as any).body //obs a parameter in a signature isn't "used"
                        && ts.isIdentifier(p.name)
                        && p.name.originalKeywordKind !== ts.SyntaxKind.ThisKeyword) { //todo: handle 'this'
                        //tslint:disable-next-line no-unused-anything
                        this.handleSymbol(p, this.checker.getSymbolAtLocation(p.name)!);
                    }
                    break;
                }
                case ts.SyntaxKind.EnumMember: {
                    const sym = this.checker.getSymbolAtLocation((node as ts.EnumMember).name)!;
                    const flags = this.info.enumMembers.get(sym);
                    if (flags === undefined) {
                        this.addFailureAtNode(node, "UNUSED");
                    }
                    else {
                        if (!hasEnumAccessFlag(flags, EnumAccessFlags.UsedInExpression)) {
                            this.addFailureAtNode(node,
                                hasEnumAccessFlag(flags, EnumAccessFlags.Tested)
                                ? "Enum flag is compared against, but never actually used."
                                : "Enum flag is never used"); //test
                        }
                    }
                    break;
                }
                default:
                    if (isTested(node)) {
                        this.handleSymbol(node, this.checker.getSymbolAtLocation(node.name)!);
                    }
            }
            node.forEachChild(cb);
        }
        sourceFile.forEachChild(cb);
    }

    private handleSymbol(node: Tested, symbol: ts.Symbol): void {
        if (isOkToNotUse(node, symbol, this.checker, this.options.ignoreNames)) {
            return;
        }

        const info = this.info.symbolInfos.get(symbol);
        if (info === undefined) {
            this.addFailureAtNode(node, "Symbol is completely unused.");
            //test -- don't we at least create the symbol??? So this should never happen?
            return;
        }

        this.checkSymbolUses(node, symbol, info);
        this.checkCollectionUses(node, symbol, info);
    }

    private checkSymbolUses(node: Tested, symbol: ts.Symbol, info: SymbolInfo): void {
        if (!info.everUsedPublicly()) {
            const x = canBePrivate(node);
            if (x === undefined) {
                this.addFailureAtNode(node.name, "Analysis found no uses of this symbol.");
                return;
            } else if (ts.isSourceFile(x)) {
                if (!isIsImplicitlyExported(node, this.sourceFile, this.checker)) { //test implicit exports
                    this.addFailureAtNode(node.name, "Analysis found no uses in other modules; this should not be exported.");
                    return;
                }
            } else {
                //todo: test that we handle completely unused abstract method
                if (!hasModifier(node.modifiers, ts.SyntaxKind.PrivateKeyword, ts.SyntaxKind.AbstractKeyword)
                    //may need to be public due to assigning this to an interface
                    && !this.info.isPropertyUsedForAssignment(symbol, this.checker)) {
                    this.addFailureAtNode(node.name, "Analysis found no public uses; this should be private.");
                    return;
                }
            }
            return;
        }

        if (!info.everRead()) {
            //might be a fn used for a side effect.
            if (info.everUsedForSideEffect() && isFunctionLike(symbol)) {
                const sig = this.checker.getSignatureFromDeclaration(symbol.valueDeclaration as ts.SignatureDeclaration)!;
                if (!(this.checker.getReturnTypeOfSignature(sig).flags & ts.TypeFlags.Void)) {
                    this.addFailureAtNode(node.name, "This function is used for its side effect, but the return value is never used.");
                }
            } else {
                if (info.mutatesCollection()) {
                    if (!info.has(AccessFlags.CreateAlias)) {
                        this.addFailureAtNode(
                            node.name,
                            "This collection is created and filled with values, but never read from.");
                    }
                }
                else {
                    if (!this.info.isPropertyUsedForAssignment(symbol, this.checker)) {
                        this.addFailureAtNode(
                            node.name,
                            "This symbol is given a value, but analysis found no reads. This is likely dead code.");
                    }
                }
            }
            return;
        }

        if (!info.everWritten() && isMutable(node)) {
            this.addFailureAtNode(
                node.name,
                ts.isVariableDeclaration(node)
                    ? "Analysis found no mutable uses of this variable; use `const`."
                    : "Analysis found no writes to this property; mark it as `readonly`.");
        }

        if (!info.everCreatedOrWritten() && !this.info.symbolIsPropertyOfTypeAssignedToSomehow(symbol, this.checker)) {
            this.addFailureAtNode(
                node.name,
                "Analysis found places where this is read, but found no places where it is given a value.");
        }

        //todo: test that we warn for something never used at all...
    }

    private checkCollectionUses(node: Tested, symbol: ts.Symbol, symbolInfo: SymbolInfo): void {
        if (symbolInfo.everUsedAsMutableCollection()) {
            return;
        }

        const typeNode = getTypeNode(node);
        let info = typeNode === undefined
            ? shouldTypeReadonlyCollectionWithInferredType(node)
                ? getMutableCollectionTypeFromType(this.checker.getTypeAtLocation(node.name))
                : undefined
            : getMutableCollectionTypeFromNode(typeNode);
        if (info === undefined) {
            return;
        }

        //For a property: but this type might be assigned to another type, and that other type might have this property mutable (test)
        //todo: also for method (test)
        if (isSymbolFlagSet(symbol, ts.SymbolFlags.Property) && this.info.isPropertyAssignedToMutableCollection(symbol, this.checker)) {
            return;
        }

        this.addFailureAtNode(
            typeof info === "string" ? node.name : info.typeNode,
            preferReadonly(typeof info === "string" ? info : info.typeName));
    }
}

//!
function preferReadonly(typeName: string): string {
    return `Analysis indicates this could be a Readonly${typeName}.`;
}

// For class property or export, warn if the implicit type is ReadonlyArray.
// But for a local variable, don't demand a type annotation everywhere an array isn't mutated, because that's annoying.
function shouldTypeReadonlyCollectionWithInferredType(node: Tested): boolean {//test
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
    if (type.flags & ts.TypeFlags.UnionOrIntersection) { //isUnionType helper
        return find((type as ts.UnionOrIntersectionType).types, getMutableCollectionTypeFromType);
    }
    if (isTypeReference(type)) {
        const { symbol } = type.target;
        if (symbol !== undefined && isMutableCollectionTypeNameStr(symbol.name)) {
            return symbol.name;
        }
    }
    return undefined;
}

//mostly dup of above, break out a utility?
export function isReadonlyType(type: ts.Type): boolean {
    if (type.flags & ts.TypeFlags.UnionOrIntersection) { //isUnionType helper
        return (type as ts.UnionOrIntersectionType).types.some(isReadonlyType);
    }
    if (isTypeReference(type)) {
        const { symbol } = type.target;
        return symbol !== undefined && symbol.name.startsWith("Readonly");
    }
    return false;
}


function getMutableCollectionTypeFromNode(typeNode: ts.TypeNode): { readonly typeNode: ts.TypeNode, readonly typeName: string } | undefined {
    return ts.isUnionTypeNode(typeNode)
        //Report errors if `string | string[]` is used immutably (should be `string | ReadonlyArray<string>`) (test)
        ? find(typeNode.types, getMutableCollectionTypeFromNode)
        : ts.isArrayTypeNode(typeNode)
        ? { typeNode, typeName: "Array" }
        : ts.isTypeReferenceNode(typeNode) && isMutableCollectionTypeName(typeNode.typeName)
        ? { typeNode, typeName: typeNode.typeName.text }
        : undefined;
}

//test: that we handle unions
function getTypeNode(node: Tested): ts.TypeNode | undefined {
    switch (node.kind) {
        case ts.SyntaxKind.Parameter:
        //Can't recommend `...args: ReadonlyArray<x>` since that's not allowed by TS
            return (node as ts.ParameterDeclaration).dotDotDotToken === undefined ? (node as ts.ParameterDeclaration).type : undefined;
        case ts.SyntaxKind.VariableDeclaration:
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.MethodDeclaration:
        case ts.SyntaxKind.MethodSignature:
        case ts.SyntaxKind.FunctionDeclaration:
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.PropertySignature:
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
            return (node as ts.VariableDeclaration | ts.PropertyDeclaration | ts.MethodDeclaration | ts.MethodSignature | ts.FunctionDeclaration | ts.PropertyDeclaration | ts.PropertySignature | ts.GetAccessorDeclaration | ts.SetAccessorDeclaration).type;
        case ts.SyntaxKind.TypeAliasDeclaration:
        case ts.SyntaxKind.EnumDeclaration:
        case ts.SyntaxKind.InterfaceDeclaration:
        case ts.SyntaxKind.ModuleDeclaration:
            return undefined;
        default:
            throw new Error(`TODO: handle ${ts.SyntaxKind[node.kind]}`)
    }
}


//reuse
function isFunctionLike(symbol: ts.Symbol): boolean {
    return isSymbolFlagSet(symbol, ts.SymbolFlags.Function | ts.SymbolFlags.Method);
}

//note: returns false for things that can't be made immutable (parameters, catch clause variables, methods)
function isMutable(node: Tested): boolean {
    if (ts.isPropertyDeclaration(node) || ts.isPropertySignature(node) || ts.isParameterPropertyDeclaration(node)) {
        return !hasModifier(node.modifiers, ts.SyntaxKind.ReadonlyKeyword);
    } else if (ts.isVariableDeclaration(node)) {
        const parent = node.parent!;
        return parent.kind !== ts.SyntaxKind.CatchClause && getVariableDeclarationKind(parent) !== VariableDeclarationKind.Const;
    } else {
        return false;
    }
}

function isOkToNotUse(node: Tested, symbol: ts.Symbol, checker: ts.TypeChecker, ignoreNames: ReadonlySet<string>): boolean {
    return isAmbient(node)
        || isIgnored(node)
        || isDeprecated(node, symbol, checker)
        || isOverload(node, symbol, checker)
        || isOverride(node, checker)
        || ignoreNames.has(node.name.text);
}

function isAmbient(node: Tested): boolean { //test
    return isNodeFlagSet(node, (ts.NodeFlags as any).Ambient) //todo: make that flag public
}

function isIgnored(node: Tested): boolean {
    return node.name.text.startsWith("_");
}

function isOverload(node: Tested, symbol: ts.Symbol, checker: ts.TypeChecker): boolean {
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

//test: for parameter property in deprecated class
//really, all we care is that some parent node has an `@deprecated` commment, why bother w/ symbols here...
function isDeprecated(node: Tested, symbol: ts.Symbol, checker: ts.TypeChecker): boolean {
    if (isItDeprecated(symbol)) {
        return true;
    }
    // If the class is deprecated, all of its methods are too.
    const parent = node.parent!;
    return (ts.isClassLike(parent) || ts.isInterfaceDeclaration(parent))
        && parent.name !== undefined
        && isItDeprecated(checker.getSymbolAtLocation(parent.name)!);
}
function isItDeprecated(symbol: ts.Symbol): boolean { //name
    return getSymbolDeprecation(symbol) !== undefined;
}

function isOverride(node: Tested, checker: ts.TypeChecker): boolean { //test
    const name = node.name.text;
    const parent = node.parent!;
    if (!ts.isClassLike(parent)) {
        return false;
    }
    return parent.heritageClauses !== undefined && parent.heritageClauses.some(clause =>
        clause.types.some(typeNode =>
            checker.getPropertyOfType(checker.getTypeFromTypeNode(typeNode), name) !== undefined));
}

function getThePropertySymbol(p: ts.ParameterDeclaration, checker: ts.TypeChecker) { //name
    return checker.getSymbolsOfParameterPropertyDeclaration(p, (p.name as ts.Identifier).text)
        .find(s => isSymbolFlagSet(s, ts.SymbolFlags.Property))!;
}


function isIsImplicitlyExported(node: Tested, sourceFile: ts.SourceFile, checker: ts.TypeChecker): boolean {
    switch (node.kind) {
        case ts.SyntaxKind.TypeAliasDeclaration:
        case ts.SyntaxKind.InterfaceDeclaration:
        case ts.SyntaxKind.EnumDeclaration:
            return isTypeImplicitlyExported(checker.getSymbolAtLocation(node.name)!, sourceFile, checker);
        case ts.SyntaxKind.VariableDeclaration:
            const s = checker.getSymbolAtLocation(node.name);
            //may be used in `typeof x` (test)
            return sourceFile.forEachChild(function cb(child): boolean | undefined {
                return ts.isTypeQueryNode(child)
                    ? checker.getSymbolAtLocation(child.exprName) === s
                    : child.forEachChild(cb);
            }) === true;
        default:
            return false;
    }
}


function isTypeImplicitlyExported(typeSymbol: ts.Symbol, sourceFile: ts.SourceFile, checker: ts.TypeChecker): boolean {
    return sourceFile.forEachChild(function cb(child): boolean | undefined {
        if (isImportLike(child)) {
            return false;
        }
        const type = getImplicitType(child, checker);
        // TODO: checker.typeEquals https://github.com/Microsoft/TypeScript/issues/13502
        return type !== undefined && checker.typeToString(type) === checker.symbolToString(typeSymbol) || child.forEachChild(cb);
    }) === true;
}

//mv
/**
 * Ignore this import if it's used as an implicit type somewhere.
 * Workround for https://github.com/Microsoft/TypeScript/issues/9944
 */
export function isImportUsed(importSpecifier: ts.Identifier, sourceFile: ts.SourceFile, checker: ts.TypeChecker): boolean {
    const importedSymbol = checker.getSymbolAtLocation(importSpecifier);
    if (importedSymbol === undefined) {
        return false;
    }

    const symbol = checker.getAliasedSymbol(importedSymbol);
    if (!isSymbolFlagSet(symbol, ts.SymbolFlags.Type)) {
        return false;
    }

    return isTypeImplicitlyExported(symbol, sourceFile, checker);
}

function getImplicitType(node: ts.Node, checker: ts.TypeChecker): ts.Type | undefined {
    if ((ts.isPropertyDeclaration(node) || ts.isVariableDeclaration(node)) &&
        node.type === undefined && node.name.kind === ts.SyntaxKind.Identifier ||
        ts.isBindingElement(node) && node.name.kind === ts.SyntaxKind.Identifier) {
        return checker.getTypeAtLocation(node);
    } else if (isSignatureDeclaration(node) && node.type === undefined) {
        const sig = checker.getSignatureFromDeclaration(node);
        return sig === undefined ? undefined : sig.getReturnType();
    } else {
        return undefined;
    }
}

export type ImportLike = ts.ImportDeclaration | ts.ImportEqualsDeclaration;
export function isImportLike(node: ts.Node): node is ImportLike {
    return node.kind === ts.SyntaxKind.ImportDeclaration || node.kind === ts.SyntaxKind.ImportEqualsDeclaration;
}
