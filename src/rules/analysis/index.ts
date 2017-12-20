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

import {
    isSymbolFlagSet,
    isTypeFlagSet,
    isExpression,
    isTypeReference,
    isUnionOrIntersectionType,
    hasModifier,
} from "tsutils";
import * as ts from "typescript";

import { EnumUse, getEnumUse } from "./enumUse";
import { Use, getUse, SymbolUses, isOnLeftHandSideOfDestructuring } from "./use";
import { multiMapAdd, skipAlias, zip, createIfNotSet } from "./utils";
import {
    getPropertySymbolOfObjectBindingPatternWithoutPropertyName,
    getContainingObjectLiteralElement,
    getPropertySymbolsFromContextualType,
} from './referencesUtils';
import { isPublicAccess } from './privacy';
import { isReadonlyType } from '../noUnusedAnythingRule';
import { find } from '../../utils';

const infoCache = new WeakMap<ts.Program, AnalysisResult>();
export function getInfo(program: ts.Program): AnalysisResult {
    return createIfNotSet(infoCache, program, () => {
        const analyzer = new Analyzer(program.getTypeChecker());
        for (const file of program.getSourceFiles()) {
            analyzer.analyze(file, file, undefined);
        }
        return analyzer.finish();
    });
}

export class AnalysisResult {
    constructor(
        private readonly symbolUses: ReadonlyMap<ts.Symbol, SymbolUses>,
        private readonly enumMembers: ReadonlyMap<ts.Symbol, EnumUse>) {}

    public getSymbolUses(symbol: ts.Symbol): SymbolUses {
        return this.symbolUses.get(symbol)!;
    }

    public getEnumAccessFlags(enumMember: ts.Symbol): EnumUse {
        const flags = this.enumMembers.get(enumMember);
        return flags === undefined ? EnumUse.None : flags;
    }
}

class Analyzer {
    private readonly symbolUses = new Map<ts.Symbol, SymbolUses>();
    private readonly enumMembers = new Map<ts.Symbol, EnumUse>();
    private readonly localVariableAliases = new Map<ts.Symbol, Set<ts.Symbol>>();
    private readonly typeAssignments = new Map</*source*/ ts.Type, /*taret*/ Set<ts.Type>>();
    private readonly seenTypeCasts = new Set<ts.Type>();

    constructor(private readonly checker: ts.TypeChecker) {}

    public finish(): AnalysisResult {
        this.addUsesFromAliases();
        this.addUsesFromAssignments();
        return new AnalysisResult(this.symbolUses, this.enumMembers);
    }

    public analyze(node: ts.Node, currentFile: ts.SourceFile, currentClass: ts.ClassLikeDeclaration | undefined) {
        switch (node.kind) {
            case ts.SyntaxKind.Identifier:
                this.trackUse(node as ts.Identifier, currentFile, currentClass);
                break;
            case ts.SyntaxKind.VariableDeclaration: {
                const { initializer, type } = node as ts.VariableDeclaration;
                if (initializer !== undefined && type !== undefined) {
                    this.addTypeAssignment(this.checker.getTypeFromTypeNode(type), this.checker.getTypeAtLocation(initializer));
                }
                break;
            }
            case ts.SyntaxKind.BinaryExpression: {
                const { left, operatorToken, right } = node as ts.BinaryExpression;
                if (operatorToken.kind === ts.SyntaxKind.EqualsToken) {
                    //But not for destructuring
                    if (!ts.isObjectLiteralExpression(left) && !ts.isArrayLiteralExpression(left)) {
                        this.addTypeAssignment(
                            this.checker.getTypeAtLocation(left),
                            this.checker.getTypeAtLocation(right));
                    }
                }
                break;
            }
            case ts.SyntaxKind.ClassDeclaration:
            case ts.SyntaxKind.ClassExpression:
                node.forEachChild(child => this.analyze(child, currentFile, node as ts.ClassLikeDeclaration));
                return;
            case ts.SyntaxKind.TypeAssertionExpression:
            case ts.SyntaxKind.AsExpression:
            case ts.SyntaxKind.TypePredicate:
                this.addCastToTypeNode((node as ts.AsExpression | ts.TypeAssertion | ts.TypePredicateNode).type);
        }

        if (isExpression(node)) {
            //TypeScript will make a bogus contextual type of `{ x: any }` in this case:
            //const { x: y } = obj;
            //The contextual type should have `{ readonly x }`. But just ignore it anyway.
            const parent = node.parent!;
            //We already handle `addTypeAssignment` at VariableDeclaration and BinaryExpression above.
            //Be sure not to get a contextual type from a destructuring --
            //for `const { x } = o;` it will have a contextual type of `{ x: any }`, which is bad since `x` is mutable.
            if (!ts.isVariableDeclaration(parent) && !ts.isBinaryExpression(parent)) {
                const ctx = this.checker.getContextualType(node);
                if (ctx !== undefined) {
                    // The type of this expression is being assigned to the contextual type.
                    this.addTypeAssignment(ctx, this.checker.getTypeAtLocation(node));
                }
            }
        }

        node.forEachChild(child => this.analyze(child, currentFile, currentClass));
    }

    /** When we assign `x = y`, if `x` is a mutable collection then `y` must be too. */
    private addUsesFromAliases(): void {
        let hadChanges = true;
        while (hadChanges) {
            hadChanges = false;
            this.localVariableAliases.forEach((aliases, sym) => {
                const originalInfo = this.symbolUses.get(sym)!;
                if (originalInfo.everUsedAsMutableCollection) {
                    return; // Nothing to propagate
                }
                for (const alias of aliases) {
                    const aliasInfo = this.symbolUses.get(alias)!;
                    // TODO: We might choose to warn on a collection which is mutated privately but publicly is only read.
                    if (aliasInfo.everUsedAsMutableCollection) {
                        originalInfo.private |= Use.ReadWithMutableType;
                        originalInfo.public |= Use.ReadWithMutableType;
                        hadChanges = true;
                    }
                }
            });
        }
    }

    /** When we assign to a type T from a type U, the properties in U are read and the corresponding properties in T are written. */
    private addUsesFromAssignments(): void {
        for (const [source, targets] of this.typeAssignments) {
            for (const target of targets) {
                this.addUsesFromAssignment(source, target);
            }
        }
    }

    private addUsesFromAssignment(source: ts.Type, target: ts.Type): void {
        if (isTypeFlagSet(source, ts.TypeFlags.Any)) {
            // Assigning to a target from "any" counts as a creation of all of `target`'s properties.
            for (const targetProperty of this.checker.getPropertiesOfType(target)) {
                this.getSymbolUses(targetProperty).public |= Use.CreateAlias;
            }
            return;
        }

        for (let sourceProperty of this.checker.getPropertiesOfType(source)) {
            sourceProperty = skipTransient(sourceProperty);
            const targetProperty = this.checker.getPropertyOfType(target, sourceProperty.name);
            if (targetProperty === undefined) {
                // Source property not actually used.
                continue;
            }

            const targetPropertyType = getTypeOfProperty(targetProperty, this.checker);
            // This counts as a read of the source property, and a creation of the target property.

            //inline
            const flag =
                // If we assign this to a mutable collection type, then it needs to be mutable too.
                (targetPropertyType === undefined || isReadonlyType(targetPropertyType) ? Use.ReadReadonly : Use.ReadWithMutableType)
                // If we assign this to a mutable property, then it needs to be mutable too.
                | (isReadonlyProperty(targetProperty) ? Use.None : Use.Write);
            this.getSymbolUses(sourceProperty).public |= flag;
            this.getSymbolUses(targetProperty).public |= Use.CreateAlias;
        }
    }

    private getSymbolUses(symbol: ts.Symbol): SymbolUses {
        return createIfNotSet(this.symbolUses, symbol, () => new SymbolUses());
    }

    private trackUse(node: ts.Identifier, currentFile: ts.SourceFile, currentClass: ts.ClassLikeDeclaration | undefined): void {
        const sym = this.checker.getSymbolAtLocation(node);
        if (sym === undefined) {
            return;
        }

        const symbol = skipTransient(skipAlias(sym, this.checker));
        if (isSymbolFlagSet(symbol, ts.SymbolFlags.EnumMember)) {
            this.enumMembers.set(symbol, getEnumUse(node) | this.enumMembers.get(symbol)!);
            return;
        }

        const destructedPropertySymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, this.checker);
        if (destructedPropertySymbol !== undefined) {
            this.trackUseOfSymbol(node, destructedPropertySymbol, currentFile, currentClass); //needs testing
            //and also track the local variable below.
        }
        else {
            const parent = node.parent!;
            const objectLiteralElement = getContainingObjectLiteralElement(node); //needs testing
            if (objectLiteralElement !== undefined) {
                if (isOnLeftHandSideOfDestructuring(objectLiteralElement.parent as ts.ObjectLiteralExpression)) {
                    const propertySymbol = this.checker.getPropertySymbolOfDestructuringAssignment(node);
                    if (propertySymbol !== undefined) {
                        this.trackUseOfSymbol(node, propertySymbol, currentFile, currentClass);
                    }

                    if (ts.isShorthandPropertyAssignment(parent)) {
                        // Also track use of the local variable
                        //todo: instead of calling into `getUse`, just handle this right here since we got the node kind already...
                        this.trackUseOfSymbol(node, this.checker.getShorthandAssignmentValueSymbol(parent)!, currentFile, currentClass);
                    }
                } else {
                    for (const assignedPropertySymbol of getPropertySymbolsFromContextualType(objectLiteralElement as ts.ObjectLiteralElement & { name: ts.Identifier }, this.checker)) { //just use optional...
                        this.trackUseOfSymbol(node, assignedPropertySymbol, currentFile, currentClass);
                    }
                    //we're doing this both for the property and for the value referenced by shorthand
                    //test: function f(x) { return { x }; }
                    if (ts.isShorthandPropertyAssignment(parent)) {
                        const v = this.checker.getShorthandAssignmentValueSymbol(parent)!;
                        this.trackUseOfSymbol(node, v, currentFile, currentClass);
                    }
                }
                return;
            }
        }

        this.trackUseOfSymbol(node, symbol, currentFile, currentClass);
    }

    private trackUseOfSymbol(
        node: ts.Identifier,
        sym: ts.Symbol,
        currentFile: ts.SourceFile,
        currentClass: ts.ClassLikeDeclaration | undefined,
    ) {
        for (const symbol of this.checker.getRootSymbols(sym)) { //needs testing
            if (symbol.declarations === undefined) {
                return;
            }

            const access = getUse(node, symbol, this.checker, aliasId => this.addAlias(aliasId, symbol));
            const info = this.getSymbolUses(symbol);
            if (isPublicAccess(node, symbol, currentFile, currentClass)) {
                info.public |= access;
            } else {
                info.private |= access;
            }
        }
    }

    private addAlias(aliasId: ts.Identifier, symbol: ts.Symbol): void {
        multiMapAdd(this.localVariableAliases, symbol, this.checker.getSymbolAtLocation(aliasId)!);
    }

    private addTypeAssignment(to: ts.Type, from: ts.Type): void {
        if (to === from) {
            return;
        }

        // If 'to' is a union, assign from 'from' to each member of the union.
        if (isUnionOrIntersectionType(to)) {
            for (const type of to.types) {
                this.addTypeAssignment(type, from);
            }
            return;
        }
        if (isUnionOrIntersectionType(from)) {
            for (const type of from.types) {
                this.addTypeAssignment(to, type);
            }
            return;
        }

        // Assigning A[] to B[] means assigning A to B.
        if (isTypeReference(to) && isTypeReference(from)) {
            if (to.typeArguments !== undefined
                && from.typeArguments !== undefined
                && to.typeArguments.length === from.typeArguments.length ) {
                zip(to.typeArguments, from.typeArguments!, (argA, argB) => {
                    this.addTypeAssignment(argA, argB);
                });
            }
        }

        multiMapAdd(this.typeAssignments, from, to);
    }

    private addCastToTypeNode(node: ts.TypeNode): void {
        this.addCastToType(this.checker.getTypeAtLocation(node));
    }

    private addCastToType(type: ts.Type): void {
        if (this.seenTypeCasts.has(type)) {
            return;
        }
        this.seenTypeCasts.add(type);

        if (isUnionOrIntersectionType(type)) {
            //test -- for `type T = { a } & { b }` we treat both `a` and `b` as implicitly-created if there is a cast to `T`
            for (const t of type.types) {
                this.addCastToType(t);
            }
        } else if (isTypeReference(type) && type.typeArguments !== undefined) {
            this.addCastToType(type.target);
            for (const t of type.typeArguments) { //name
                this.addCastToType(t);
            }
        } else {
            for (let prop of this.checker.getPropertiesOfType(type)) {
                prop = skipTransient(prop);
                // Casting to a type counts as a creation of each property.
                this.getSymbolUses(prop).public |= Use.CreateAlias;

                // TODO: `this.checker.getDeclaredTypeOfSymbol(prop)` ought to work...
                // but it returns 'any' for `readonly a: { readonly b: number }`.
                if (prop.declarations !== undefined) {
                    for (const d of prop.declarations!) {
                        if ((ts.isPropertyDeclaration(d) || ts.isPropertySignature(d)) && d.type !== undefined) {
                            this.addCastToTypeNode(d.type);
                        }
                    }
                }
            }
        }
    }
}

function skipTransient(symbol: ts.Symbol): ts.Symbol {
    // TODO: Maybe TypeScript shouldn't be returning transient symbols from the public API?
    if (isSymbolFlagSet(symbol, ts.SymbolFlags.Transient)) {
        const target = (symbol as any).target;
        return target === undefined ? symbol : target;
    } else {
        return symbol;
    }
}

function getTypeOfProperty(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Type | undefined {
    return symbol.declarations === undefined ? undefined : find(symbol.declarations, d =>
        (ts.isPropertyDeclaration(d) || ts.isPropertySignature(d)) && d.type !== undefined
            ? checker.getTypeFromTypeNode(d.type)
            : undefined);
}

function isReadonlyProperty(propertySymbol: ts.Symbol): boolean {
    // TODO: Would be nice if TypeScript's `isReadonlySymbol` were public...
    return propertySymbol.declarations !== undefined
        && propertySymbol.declarations.some(d => hasModifier(d.modifiers, ts.SyntaxKind.ReadonlyKeyword));
}
