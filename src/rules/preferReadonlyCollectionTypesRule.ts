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

import { isTested, PropertyDeclarationLike, Tested, Info, getInfo } from './analysis';
import { isOkToNotUse } from './noUnusedAnythingRule';
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
        return this.applyWithFunction(sourceFile, ctx => walk(ctx, getInfo(program), program.getTypeChecker()));
    }
}

function walk(ctx: Lint.WalkContext<void>, info: Info, checker: ts.TypeChecker): void {
    ctx.sourceFile.forEachChild(function cb(node) {
        if (isTested(node)) {
            foo(node);
        }
        node.forEachChild(cb);
    });

    function foo(node: Tested): void {//name
        const symbol = checker.getSymbolAtLocation(node.name)!;
        if (isOkToNotUse(node, symbol, checker)) {
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
        if (x && !info.symbolToTypeIsMutated.has(symbol)) {
            ctx.addFailureAtNode(x.node, `Prefer Readonly${x.name}`);
        }
    }
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

//test: that we handle unions
function getTypeNode(node: Tested | PropertyDeclarationLike): ts.TypeNode | undefined {
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
        default:
            throw new Error(`TODO: handle ${ts.SyntaxKind[node.kind]}`)
    }
}
