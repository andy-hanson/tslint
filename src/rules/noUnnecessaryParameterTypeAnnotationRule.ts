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

import * as ts from "typescript";

import * as Lint from "../index";
import { findDefined } from "../utils";

export class Rule extends Lint.Rules.TypedRule {
    /* tslint:disable:object-literal-sort-keys */
    public static metadata: Lint.IRuleMetadata = {
        ruleName: "no-unnecessary-parameter-type-annotation",
        description: "",
        rationale: Lint.Utils.dedent`!`,
        optionsDescription: "Not configurable.",
        options: null,
        optionExamples: [true],
        type: "typescript",
        typescriptOnly: true,
    };
    /* tslint:enable:object-literal-sort-keys */

    public static FAILURE_STRING = "Function has a contextual type; do not specify types!";

    public applyWithProgram(sourceFile: ts.SourceFile, program: ts.Program): Lint.RuleFailure[] {
        return this.applyWithFunction(sourceFile, (ctx) => walk(ctx, program.getTypeChecker()));
    }
}

function walk(ctx: Lint.WalkContext<void>, checker: ts.TypeChecker) {
    ts.forEachChild(ctx.sourceFile, function cb(node) {
        if (ts.isContextualSignatureNode(node)) {
            check(node);
        }
        ts.forEachChild(node, cb);
    });

    function check(node: ts.ContextualSignatureNode): void {
        const typeNode = findDefined(node.parameters, (p) => p.type);
        if (typeNode === undefined) { return; }
        const s = checker.getContextualSignature(node);
        if (s !== undefined) {
            ctx.addFailureAtNode(typeNode, Rule.FAILURE_STRING);
        }
    }
}
