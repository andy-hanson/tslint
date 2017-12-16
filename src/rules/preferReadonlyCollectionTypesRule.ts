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
            check(node);
        }
        node.forEachChild(cb);
    });

    function check(node: Tested): void {
        const symbol = checker.getSymbolAtLocation(node.name)!;
        if (isOkToNotUse(node, symbol, checker, new Set())) {
            return;
        }

        const typeNode = getTypeNode(node);
        const mutableTypeInfo = typeNode === undefined ? undefined : getMutableCollectionType(typeNode);
        if (mutableTypeInfo === undefined) {
            return;
        }

        const symbolInfo = info.symbolInfos.get(symbol)!;
        if (symbolInfo.everUsedAsMutableCollection()) {
            return;
        }

        //For a property: but this type might be assigned to another type, and that other type might have this property mutable (test)
        //todo: also for method (test)
        if (isSymbolFlagSet(symbol, ts.SymbolFlags.Property)
            && info.isPropertyAssignedToMutableCollection(symbol, checker)) {
            return;
        }

        ctx.addFailureAtNode(mutableTypeInfo.typeNode, `Prefer Readonly${mutableTypeInfo.typeName}`);
    }
}

const x = new Set(["Array", "Set", "Map"]);//name
function getMutableCollectionType(typeNode: ts.TypeNode): { readonly typeNode: ts.TypeNode, readonly typeName: string } | undefined {
    return ts.isUnionTypeNode(typeNode)
        //Report errors if `string | string[]` is used immutably (should be `string | ReadonlyArray<string>`) (test)
        ? find(typeNode.types, getMutableCollectionType)
        : ts.isArrayTypeNode(typeNode)
        ? { typeNode, typeName: "Array" }
        : ts.isTypeReferenceNode(typeNode) && typeNode.typeName.kind === ts.SyntaxKind.Identifier && x.has(typeNode.typeName.text)
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
