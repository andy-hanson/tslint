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

import * as ts from "typescript";
import { isSymbolFlagSet } from 'tsutils';

// For now, only track declarations with identifier names.
export type UsageTrackedDeclaration = ts.NamedDeclaration & { readonly name: ts.Identifier };
export function isUsageTrackedDeclaration(node: ts.Node): node is UsageTrackedDeclaration {
    switch (node.kind) {
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.PropertySignature:
        case ts.SyntaxKind.MethodSignature:
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
        case ts.SyntaxKind.ModuleDeclaration:
            type T =
                | ts.PropertyDeclaration | ts.PropertySignature | ts.MethodSignature
                | ts.GetAccessorDeclaration | ts.SetAccessorDeclaration | ts.ModuleDeclaration;
            return ts.isIdentifier((node as T).name);
        case ts.SyntaxKind.MethodDeclaration:
            const parent = node.parent!;
            return !ts.isObjectLiteralExpression(parent) && ts.isIdentifier((node as ts.MethodDeclaration).name);
        case ts.SyntaxKind.InterfaceDeclaration:
        case ts.SyntaxKind.EnumDeclaration:
        case ts.SyntaxKind.TypeAliasDeclaration:
            return true;
        case ts.SyntaxKind.FunctionDeclaration:
            return (node as ts.FunctionDeclaration).name !== undefined;
        case ts.SyntaxKind.VariableDeclaration:
            return ts.isIdentifier((node as ts.VariableDeclaration).name);
        default:
            return false;
    }
}

export function skipAlias(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Symbol {
    const alias = tryGetAliasedSymbol(symbol, checker);
    return alias === undefined ? symbol : alias;
}

export function tryGetAliasedSymbol(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Symbol | undefined {
    return isSymbolFlagSet(symbol, ts.SymbolFlags.Alias) ? checker.getAliasedSymbol(symbol) : undefined;
}

export function multiMapAdd<K, V>(map: Map<K, V[]>, key: K, value: V): void {
    const values = map.get(key);
    if (values === undefined) {
        map.set(key, [value]);
    } else {
        values.push(value);
    }
}

export function assertNever(value: never): never {
    throw new Error(value);
}
