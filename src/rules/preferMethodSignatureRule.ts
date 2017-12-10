/**
 * @license
 * Copyright 2017 Palantir Technologies, Inc.
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

import { isFunctionTypeNode, isPropertySignature } from "tsutils";
import * as ts from "typescript";

import * as Lint from "../index";

export class Rule extends Lint.Rules.AbstractRule {
    /* tslint:disable:object-literal-sort-keys */
    public static metadata: Lint.IRuleMetadata = {
        ruleName: "prefer-method-signature",
        description: "Prefer `foo(): void` over `foo: () => void` in interfaces and types.",
        hasFix: true,
        optionsDescription: "Not configurable.",
        options: null,
        optionExamples: [true],
        type: "style",
        typescriptOnly: false,
    };
    /* tslint:enable:object-literal-sort-keys */

    public static readonly FAILURE_STRING = "Use a method signature instead of a property signature of function type.";

    public apply(sourceFile: ts.SourceFile): Lint.RuleFailure[] {
        return this.applyWithFunction(sourceFile, walk);
    }
}

function walk(ctx: Lint.WalkContext<void>): void {
    return ts.forEachChild(ctx.sourceFile, function cb(node: ts.Node): void {
        if (isPropertySignature(node)) {
            const { type } = node;
            if (type !== undefined && isFunctionTypeNode(type)) {
                ctx.addFailureAtNode(node.name, Rule.FAILURE_STRING, type.type === undefined ? undefined : [
                    Lint.Replacement.deleteFromTo(type.pos - 1, type.getStart()), // delete colon in 'foo: () => void'
                    Lint.Replacement.replaceFromTo(type.parameters.end + 1, type.type.pos, ":"), // replace => with colon
                ]);
            }
        }
        return ts.forEachChild(node, cb);
    });
}
