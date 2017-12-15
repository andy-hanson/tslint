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

import * as ts from "typescript";

import * as Lint from "..";
import { find } from "../utils";

import { getInfo, isTested, AnalysisResult, Tested } from "./analysis";
import { isOkToNotUse } from "./noUnusedAnythingRule";
import { isSymbolFlagSet } from "tsutils"; //todo: import fix is using single quotes for some reason...

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
        return this.applyWithFunction(sourceFile, ctx => walk(ctx, getInfo(program), program.getTypeChecker()));
    }
}

function walk(ctx: Lint.WalkContext<void>, info: AnalysisResult, checker: ts.TypeChecker): void {
    ctx.sourceFile.forEachChild(function cb(node) {
        if (isTested(node)) {
            foo(node);
        }
        node.forEachChild(cb);
    });

    function foo(node: Tested): void {//name
        const symbol = checker.getSymbolAtLocation(node.name)!;
        if (isOkToNotUse(node, symbol, checker, new Set())) { //todo: ignorenames
            return;
        }

        //If it's an array, check use
        //todo: map and set too
        //todo: unions too
        //todo: would like a type checker api...
        //const actualType = this.checker.getTypeAtLocation(node.name);
        //if (this.checker.typeToString(actualType).endsWith("[]")) {
        const typeNode = getTypeNode(node);
        const x = typeNode && getMutableCollectionType(typeNode);
        if (!x) {
            return;
        }

        const y = info.symbolInfos.get(symbol)!;
        if (y.everUsedAsMutableCollection()) {
            return;
        }

        //For a property: but this type might be assigned to another type, and that other type might have this property mutable (test)
        //todo: also for method
        if (isSymbolFlagSet(symbol, ts.SymbolFlags.Property) && isPropertyAssignedToMutableCollection(symbol, checker, info)) {
            return;
        }

        ctx.addFailureAtNode(x.node, `Prefer Readonly${x.name}`);
    }
}

//method in AnalysisResult
function isPropertyAssignedToMutableCollection(symbol: ts.Symbol, checker: ts.TypeChecker, info: AnalysisResult): boolean {
    const targets = info.typeAssignmentsSourceToTarget.get(getTypeContainingProperty(symbol, checker));
    if (!targets) {
        return false;
    }

    for (const target of targets) {
        const targetProperty = checker.getPropertyOfType(target, symbol.name);
        if (targetProperty) {
            //test
            if (!allowsReadonlyCollectionType(checker.getDeclaredTypeOfSymbol(targetProperty))) {
                return true;
            }
        }
    }

    return false;
}

//mv to analysisresult class
export function isPropertyAssignedIndirectly(symbol: ts.Symbol, checker: ts.TypeChecker, info: AnalysisResult): boolean {
    //find things that are assigned to this type.
    const sources = info.typeAssignmentsTargetToSource.get(getTypeContainingProperty(symbol, checker));
    if (!sources) {
        return false;
    }

    for (const source of sources) {//name
        if (source.flags & ts.TypeFlags.Any) {
            return true;
        }
        else {
            const sourceProperty = checker.getPropertyOfType(source, symbol.name);
            if (sourceProperty) {
                return true;
            }
        }
    }
    return false;
}

export function getParentOfPropertySymbol(symbol: ts.Symbol): ts.Symbol {
    return (symbol as any).parent;
}

//todo; better
function getTypeContainingProperty(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Type {
    const parentSymbol = (symbol as any).parent as ts.Symbol;
    return checker.getTypeAtLocation(parentSymbol.declarations![0]);

    /*for (const d of symbol.declarations!) {
        if (ts.isPropertyDeclaration(d) || ts.isPropertySignature(d)) {
            const parent = d.parent as ts.ClassDeclaration | ts.ClassExpression | ts.InterfaceDeclaration | ts.TypeLiteralNode;
            switch (parent.kind) {
                case ts.SyntaxKind.ClassDeclaration:
                case ts.SyntaxKind.ClassExpression:
                case ts.SyntaxKind.InterfaceDeclaration:
                case ts.SyntaxKind.TypeLiteral:
                    break;
                default:
                    throw new Error(); //!
            }
            checker.getSymbolAtLocation(ts.isTypeLiteralNode(parent) ? parent : parent.name);
            checker.getTypeAtLocation(parent.name!);
        }
    }*/
}

const x = new Set(["Array", "Set", "Map"]);
function getMutableCollectionType(node: ts.TypeNode): { readonly node: ts.TypeNode, readonly name: string } | undefined {
    if (ts.isUnionTypeNode(node)) {
        //Report errors if `string | string[]` is used immutably (should be `string | ReadonlyArray<string>`)
        return find(node.types, getMutableCollectionType);
    }
    else if (ts.isArrayTypeNode(node)) {
        return { node, name: "Array" };
    }
    else if (ts.isTypeReferenceNode(node) && node.typeName.kind === ts.SyntaxKind.Identifier && x.has(node.typeName.text)) {
        return { node, name: node.typeName.text };
    }
    return undefined;
}

function allowsReadonlyCollectionType(type: ts.Type): boolean {
    if (type.flags & ts.TypeFlags.Union) {//preferconditional
        return (type as ts.UnionType).types.some(allowsReadonlyCollectionType);
    }
    return type.symbol !== undefined && type.symbol.name.startsWith("Readonly");
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
            return undefined;
        default:
            throw new Error(`TODO: handle ${ts.SyntaxKind[node.kind]}`)
    }
}
