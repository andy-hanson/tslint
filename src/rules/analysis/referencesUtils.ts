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
import { isSymbolFlagSet, isUnionType } from "tsutils";
import * as ts from "typescript";

import { tryCast } from "./utils";

// TODO: These utilities are from findAllReferences.ts in the TypeScript repo -- they could be made public.

function getObjectBindingElementWithoutPropertyName(symbol: ts.Symbol): ts.BindingElement | undefined {
    const bindingElement = symbol.declarations === undefined ? undefined : symbol.declarations.find(ts.isBindingElement);
    return bindingElement !== undefined
        && bindingElement.parent!.kind === ts.SyntaxKind.ObjectBindingPattern
        && bindingElement.propertyName === undefined
        ? bindingElement
        : undefined;
}

export function getPropertySymbolOfObjectBindingPatternWithoutPropertyName(
    symbol: ts.Symbol,
    checker: ts.TypeChecker,
): ts.Symbol | undefined {
    const bindingElement = getObjectBindingElementWithoutPropertyName(symbol);
    if (bindingElement === undefined) {
        return undefined;
    }

    const typeOfPattern = checker.getTypeAtLocation(bindingElement.parent!);
    const propSymbol = typeOfPattern === undefined
        ? undefined
        : checker.getPropertyOfType(typeOfPattern, (<ts.Identifier>bindingElement.name).text);
    if (propSymbol !== undefined && isSymbolFlagSet(propSymbol, ts.SymbolFlags.Accessor)) {
        // See GH#16922
        assert(isSymbolFlagSet(propSymbol, ts.SymbolFlags.Transient));
        return (propSymbol as any/*ts.TransientSymbol*/).target;
    }
    return propSymbol;
}

export function getContainingObjectLiteralElement(node: ts.Node): ts.ObjectLiteralElement | undefined {
    const parent = node.parent!;
    switch (node.kind) {
        case ts.SyntaxKind.StringLiteral:
        case ts.SyntaxKind.NumericLiteral:
            if (parent.kind === ts.SyntaxKind.ComputedPropertyName) {
                return tryCast(parent.parent!, isObjectLiteralElement);
            }
        // falls through
        case ts.SyntaxKind.Identifier:
            return isObjectLiteralElement(parent) &&
                (parent.parent!.kind === ts.SyntaxKind.ObjectLiteralExpression || parent.parent!.kind === ts.SyntaxKind.JsxAttributes) &&
                parent.name === node ? parent : undefined;
        default:
            return undefined;
    }
}

function isObjectLiteralElement(node: ts.Node): node is ts.ObjectLiteralElement {
    switch (node.kind) {
        case ts.SyntaxKind.JsxAttribute:
        case ts.SyntaxKind.JsxSpreadAttribute:
        case ts.SyntaxKind.PropertyAssignment:
        case ts.SyntaxKind.ShorthandPropertyAssignment:
        case ts.SyntaxKind.MethodDeclaration:
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
            return true;
        default:
            return false;
    }
}

/** Gets all symbols for one property. Does not get symbols for every property. */
export function getPropertySymbolsFromContextualType(node: ts.ObjectLiteralElement, checker: ts.TypeChecker): ReadonlyArray<ts.Symbol> {
    const objectLiteral = node.parent as ts.ObjectLiteralExpression;
    const name = getNameFromObjectLiteralElement(node);
    if (name === undefined) {
        return [];
    }

    const contextualType = checker.getContextualType(objectLiteral);
    if (contextualType === undefined) {
        return [];
    }

    const result: ts.Symbol[] = [];
    const symbol = contextualType.getProperty(name);
    if (symbol !== undefined) {
        result.push(symbol);
    }

    if (isUnionType(contextualType)) {
        for (const t of contextualType.types) {
            const symbol = t.getProperty(name);
            if (symbol !== undefined) {
                result.push(symbol);
            }
        }
    }
    return result;
}

function getNameFromObjectLiteralElement(node: ts.ObjectLiteralElement): string | undefined {
    const name = node.name!;
    return name.kind !== ts.SyntaxKind.ComputedPropertyName
        ? name.text
        // treat computed property names where expression is string/numeric literal as just string/numeric literal
        : isStringOrNumericLiteral(name.expression) ? name.expression.text : undefined;
}

function isStringOrNumericLiteral(node: ts.Node): node is ts.StringLiteral | ts.NumericLiteral {
    return node.kind === ts.SyntaxKind.StringLiteral || node.kind === ts.SyntaxKind.NumericLiteral;
}
