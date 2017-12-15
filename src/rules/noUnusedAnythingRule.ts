/**
 * @license
 * Copyright 2016 Palantir Technologies, Inc.
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

import { hasModifier, isSymbolFlagSet, getVariableDeclarationKind, VariableDeclarationKind } from "tsutils";
import * as ts from "typescript";

import * as Lint from "..";
import { getSymbolDeprecation } from './deprecationRule';

import { isTested, Tested, ElementOfClassOrInterface, Info, getInfo, hasEnumAccessFlag, EnumAccessFlags } from './analysis';
import { isNodeFlagSet } from 'tsutils';

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
        /*const errs = ts.getPreEmitDiagnostics(program, sourceFile);
        for (const err of errs) {
            const text = ts.flattenDiagnosticMessageText(err.messageText, "\n");
            const file = err.file!;
            const pos = file.getLineAndCharacterOfPosition(err.start!);
            console.log(file.fileName, pos, text);
        }
        assert(errs.length === 0); //kill
        */
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
        private readonly info: Info,
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

    private handleSymbol(node: Tested, sym: ts.Symbol): void {
        if (isOkToNotUse(node, sym, this.checker, this.options.ignoreNames)) {
            return;
        }

        const info = this.info.properties.get(sym);
        if (info === undefined) {
            this.addFailureAtNode(node, "UNUSED"); //!
            return;
        }

        if (!info.everUsedPublicly()) {
            if (ts.isClassElement(node)) {
                //todo: test that we handle completely unused abstract method
                if (!hasModifier(node.modifiers, ts.SyntaxKind.PrivateKeyword, ts.SyntaxKind.AbstractKeyword)) {
                    this.addFailureAtNode(node, "Class element not used publicly.");
                }
            }
            else if (hasModifier(node.modifiers, ts.SyntaxKind.ExportKeyword)) {
                this.addFailureAtNode(node, "No need to export");
            }
            else {
                this.addFailureAtNode(node, "Has no uses"); //ever happens?
            }
            return;
        }

        if (!info.everRead()) {
            this.addFailureAtNode(node, "This is given a value, but the value is never read");
            return;
        }

        if (!info.everWritten() && isMutable(node)) {
            this.addFailureAtNode(node, ts.isVariableDeclaration(node) ? "Prefer const" : "Make readonly");
        }

        if (!info.everCreatedOrWritten()) {
            this.addFailureAtNode(node, "This is read, but is never created or written to.");
        }

        //todo: test that we warn for something never used at all...
    }
}

//note: returns false for things that can't be made immutable (parameters, catch clause variables, methods)
function isMutable(node: Tested): boolean {
    if (ts.isPropertyDeclaration(node) || ts.isParameterPropertyDeclaration(node)) {
        return !hasModifier(node.modifiers, ts.SyntaxKind.ReadonlyKeyword);
    } else if (ts.isVariableDeclaration(node)) {
        const parent = node.parent!;
        return parent.kind !== ts.SyntaxKind.CatchClause && getVariableDeclarationKind(parent) !== VariableDeclarationKind.Const;
    } else {
        return false;
    }
}

function isTestedElement(node: Tested): node is ElementOfClassOrInterface {
    switch (node.kind) {
        case ts.SyntaxKind.VariableDeclaration:
        case ts.SyntaxKind.Parameter:
            return false;
        default:
            return true;
    }
}

export function isOkToNotUse(node: Tested, symbol: ts.Symbol, checker: ts.TypeChecker, ignoreNames: ReadonlySet<string>): boolean {
    return isAmbient(node)
        || isIgnored(node)
        || isDeprecated(node, symbol, checker)
        || isOverload(node, symbol, checker)
        || isTestedElement(node) && isOverride(node, checker)
        || ignoreNames.has(node.name.text);
}

function isAmbient(node: Tested): boolean { //test
    return isNodeFlagSet(node, (ts.NodeFlags as any).Ambient) //todo: make that flag public
}

function isIgnored(node: Tested): boolean {
    return node.name.text.startsWith("_");
}

function isOverload(node: Tested, symbol: ts.Symbol, checker: ts.TypeChecker): boolean {
    if (symbol.declarations!.length !== 1) {
        return true;
    }
    if (!ts.isParameter(node)) {
        return false;
    }

    const parent = node.parent!;
    const name = ts.getNameOfDeclaration(parent);
    const x = name && checker.getSymbolAtLocation(name);
    return !!x && x.declarations!.length !== 1;
}

//handle interface too
function isDeprecated(node: Tested, symbol: ts.Symbol, checker: ts.TypeChecker) {
    return getSymbolDeprecation(symbol) !== undefined
        //or the class is deprecated (thus all the members are)
        || isTestedElement(node) && node.parent.name && getSymbolDeprecation(checker.getSymbolAtLocation(node.parent.name)!) !== undefined;
}

function isOverride(node: ElementOfClassOrInterface, checker: ts.TypeChecker): boolean { //test
    const name = node.name.text;
    //const sym = checker.getSymbolAtLocation(node)!;
    const { heritageClauses } = node.parent;
    return heritageClauses !== undefined && heritageClauses.some(clause =>
        clause.types.some(typeNode =>
            checker.getPropertyOfType(checker.getTypeFromTypeNode(typeNode), name) !== undefined));
}

function getThePropertySymbol(p: ts.ParameterDeclaration, checker: ts.TypeChecker) { //name
    return checker.getSymbolsOfParameterPropertyDeclaration(p, (p.name as ts.Identifier).text)
        .find(s => isSymbolFlagSet(s, ts.SymbolFlags.Property))!;
}
