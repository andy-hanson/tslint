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
import * as ts from "typescript";

import { getEqualsKind, ancestorWhere } from "../..";

export const enum EnumAccessFlags {
    None = 0,
    Tested = 2 ** 0,
    UsedInExpression = 2 ** 1,
}
export function hasEnumAccessFlag(a: EnumAccessFlags, b: EnumAccessFlags): boolean {
    return (a & b) !== EnumAccessFlags.None;
}

export function accessFlagsForEnumAccess(node: ts.Identifier): EnumAccessFlags {
    const parent = node.parent!;
    if (ts.isPropertyAccessExpression(parent)) {
        return flagsForPropertyAccess(parent.parent!);
    } else {
        // The only place where an enum member can appear unqualified is inside the enum itself.
        // Don't count uses inside the enum itself as uses because that implies that the individual flag is never used.
        assert(ts.isEnumMember(parent) || ts.isQualifiedName(parent) || ancestorWhere(parent, ts.isEnumMember) !== undefined);
        return EnumAccessFlags.None;
    }
}

function flagsForPropertyAccess(parent: ts.Node): EnumAccessFlags {//name
    switch (parent.kind) {
        case ts.SyntaxKind.CaseClause:
            return EnumAccessFlags.Tested;
        case ts.SyntaxKind.BinaryExpression:
            return getEqualsKind((parent as ts.BinaryExpression).operatorToken) !== undefined
                ? EnumAccessFlags.Tested
                : EnumAccessFlags.UsedInExpression;
        default:
            return EnumAccessFlags.UsedInExpression;
    }
}
