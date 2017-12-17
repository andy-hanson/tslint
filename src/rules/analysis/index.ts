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
import {
    hasModifier,
    isSymbolFlagSet,
    isTypeFlagSet,
    isExpression,
    isTypeReference,
    isUnionType,
    isUnionOrIntersectionType,
} from "tsutils";
import * as ts from "typescript";

import { AccessFlags, accessFlags, SymbolInfo } from "./accessFlags";
import { EnumAccessFlags, accessFlagsForEnumAccess } from "./enumAccessFlags";
import { isUsageTrackedDeclaration, multiMapAdd, skipAlias } from "./utils";
import {
    getPropertySymbolOfObjectBindingPatternWithoutPropertyName,
    getContainingObjectLiteralElement,
    getPropertySymbolsFromContextualType,
} from './referencesUtils';

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
        readonly symbolInfos: ReadonlyMap<ts.Symbol, SymbolInfo>,
        readonly enumMembers: ReadonlyMap<ts.Symbol, EnumAccessFlags>,
        private readonly typeAssignmentsSourceToTarget: ReadonlyMap</*from*/ ts.Type, /*to*/ ReadonlyArray<ts.Type>>,
        private readonly typeAssignmentsTargetToSource: ReadonlyMap</*to*/ ts.Type, /*from*/ ReadonlyArray<ts.Type>>,
        private readonly castedToTypes: ReadonlySet<ts.Symbol>) {}

    //test
    isPropertyUsedForAssignment(symbol: ts.Symbol, checker: ts.TypeChecker): boolean {
        if (!(symbol.flags & ts.SymbolFlags.Property)) {
            return false; //todo: what about methods
        }
        const targets = this.typeAssignmentsSourceToTarget.get(getTypeContainingProperty(symbol, checker));
        return targets !== undefined && targets.some(target => checker.getPropertyOfType(target, symbol.name) !== undefined);
    }

    isPropertyAssignedToMutableCollection(symbol: ts.Symbol, checker: ts.TypeChecker): boolean {
        const targets = this.typeAssignmentsSourceToTarget.get(getTypeContainingProperty(symbol, checker));
        //TODO: use a set of target types, because there will be duplicates...
        return targets !== undefined && targets.some(target => {
            const targetProperty = checker.getPropertyOfType(target, symbol.name);
            if (targetProperty === undefined) {
                return false;
            }
            const t = getTypeOfProperty(targetProperty, checker); //name
            return t !== undefined && !allowsReadonlyCollectionType(t);
        });
    }

    symbolIsIndirectlyAssignedProperty(symbol: ts.Symbol, checker: ts.TypeChecker) {
        //why do these two get the containing type differently? Would be nice to calculate only once...
        return isSymbolFlagSet(symbol, ts.SymbolFlags.Property) &&
            (this.isParentCastedTo(symbol) || this.isPropertyAssignedIndirectly(symbol, checker));
    }

    private isParentCastedTo(symbol: ts.Symbol): boolean {
        return this.castedToTypes.has(getParentOfPropertySymbol(symbol));
    }

    private isPropertyAssignedIndirectly(propertySymbol: ts.Symbol, checker: ts.TypeChecker): boolean {
        const sources = this.typeAssignmentsTargetToSource.get(getTypeContainingProperty(propertySymbol, checker))
        return sources !== undefined && sources.some(source =>
            isTypeFlagSet(source, ts.TypeFlags.Any) || checker.getPropertyOfType(source, propertySymbol.name) !== undefined);
    }

}

//why doesn't checker.getDeclaredTypeOfSymbol work?
//this needs to work on methods too...
function getTypeOfProperty(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Type | undefined {
    if (symbol.declarations === undefined) {
        return undefined; //neater
    }
    for (const d of symbol.declarations) { //name
        if ((ts.isPropertyDeclaration(d) || ts.isPropertySignature(d)) && d.type !== undefined) {
            return checker.getTypeFromTypeNode(d.type);
        }
    }
    return undefined;
}

function allowsReadonlyCollectionType(type: ts.Type): boolean {
    return isTypeFlagSet(type, ts.TypeFlags.Any)
        || (isUnionType(type)
            ? type.types.some(allowsReadonlyCollectionType)
            : type.symbol !== undefined && type.symbol.name.startsWith("Readonly"));
}

//mv
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

class Analyzer {
    private readonly symbolInfos = new Map<ts.Symbol, SymbolInfo>(); //name
    private readonly enumMembers = new Map<ts.Symbol, EnumAccessFlags>();
    private readonly localVariableAliases = new Map<ts.Symbol, ts.Symbol[]>();
    private readonly typeAssignmentsSourceToTarget = new Map</*from*/ ts.Type, /*to*/ ts.Type[]>();
    private readonly typeAssignmentsTargetToSource = new Map</*to*/ ts.Type, /*from*/ ts.Type[]>();
    private readonly castedToTypes = new Set<ts.Symbol>();

    constructor(private readonly checker: ts.TypeChecker) {}

    public finish(): AnalysisResult {
        //todo: repeat until no changes (test)
        this.localVariableAliases.forEach((aliases, sym) => {
            const originalInfo = this.symbolInfos.get(sym)!;
            for (const alias of aliases) {
                //todo: might be a private alias...
                const aliasInfo = this.symbolInfos.get(alias)!;
                if (aliasInfo.everUsedAsMutableCollection) {//todo: public/private difference
                    originalInfo.private |= AccessFlags.ReadWithMutableType;
                    originalInfo.public |= AccessFlags.ReadWithMutableType;
                }
            }
        });

        return new AnalysisResult(
            this.symbolInfos,
            this.enumMembers,
            this.typeAssignmentsSourceToTarget,
            this.typeAssignmentsTargetToSource,
            this.castedToTypes);
    }

    public analyze(node: ts.Node, currentFile: ts.SourceFile, currentClass: ts.ClassLikeDeclaration | undefined) {
        switch (node.kind) {
            case ts.SyntaxKind.Identifier:
                this.fooId(node as ts.Identifier, currentFile, currentClass);
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
                    this.addTypeAssignment(
                        this.checker.getTypeAtLocation(left),
                        this.checker.getTypeAtLocation(right));
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

        //at any location, if there's a contextual type, it's a type assignment.
        if (isExpression(node)) {
            const ctx = this.checker.getContextualType(node);
            if (ctx !== undefined) {
                //don't need the help from accessFlags then?
                this.addTypeAssignment(ctx, this.checker.getTypeAtLocation(node));
            }
        }

        node.forEachChild(child => this.analyze(child, currentFile, currentClass));
    }

    private fooId(node: ts.Identifier, currentFile: ts.SourceFile, currentClass: ts.ClassLikeDeclaration | undefined) { //name
        const sym = this.checker.getSymbolAtLocation(node);
        if (sym === undefined) {
            return;
        }

        const symbol = skipTransient(skipAlias(sym, this.checker));
        if (isSymbolFlagSet(symbol, ts.SymbolFlags.EnumMember)) {
            this.trackEnumMemberUse(node, symbol);
        } else {
            this.trackSymbolUse(node, symbol, currentFile, currentClass);
        }
    }

    private trackEnumMemberUse(node: ts.Identifier, symbol: ts.Symbol) {//needs much testing...
        const prevFlags = this.enumMembers.get(symbol);
        const flags = accessFlagsForEnumAccess(node);
        this.enumMembers.set(symbol, flags | (prevFlags === undefined ? EnumAccessFlags.None : prevFlags));
    }

    private trackSymbolUse(
        node: ts.Identifier,
        symbol: ts.Symbol,
        currentFile: ts.SourceFile,
        currentClass: ts.ClassLikeDeclaration | undefined,
    ) {
        const destructedPropertySymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, this.checker);
        if (destructedPropertySymbol !== undefined) {
            this.trackUseOfEachRootSymbol(node, destructedPropertySymbol, currentFile, currentClass); //needs testing
            //and also track the local variable below.
        }
        else {
            const objectLiteral = getContainingObjectLiteralElement(node); //needs testing
            if (objectLiteral !== undefined) {
                for (const assignedPropertySymbol of getPropertySymbolsFromContextualType(objectLiteral, this.checker)) {
                    this.trackUseOfEachRootSymbol(node, assignedPropertySymbol, currentFile, currentClass);
                }
                //we're doing this both for the property and for the value referenced by shorthand
                //test: function f(x) { return { x }; }
                const parent = node.parent!;
                if (ts.isShorthandPropertyAssignment(parent)) {
                    const v = this.checker.getShorthandAssignmentValueSymbol(parent)!;
                    this.trackUseOfEachRootSymbol(node, v, currentFile, currentClass);
                }
                return;
            }
        }

        this.trackUseOfEachRootSymbol(node, symbol, currentFile, currentClass);
    }

    private trackUseOfEachRootSymbol(
        node: ts.Identifier,
        symbol: ts.Symbol,
        currentFile: ts.SourceFile,
        currentClass: ts.ClassLikeDeclaration | undefined,
    ) {
        for (const root of this.checker.getRootSymbols(symbol)) { //needs testing
            this.trackUse(node, root, currentFile, currentClass);
        }
    }

    private trackUse(
        node: ts.Identifier,
        symbol: ts.Symbol,
        currentFile: ts.SourceFile,
        currentClass: ts.ClassLikeDeclaration | undefined,
    ): void {
        if (symbol.declarations === undefined) {
            return;
        }

        const info = createIfNotSet(this.symbolInfos, symbol, () => new SymbolInfo());
        const access = accessFlags(node, symbol, this.checker,
            aliasId => this.addAlias(aliasId, symbol),
            //todo: probably don't need to pass that in since we handle that in the `isExpression` block in `analyze`
            (a, b) => {
                this.addTypeAssignment(a, b)
            }); //neater
        if (isPublicAccess(node, symbol, currentFile, currentClass)) {
            info.public |= access;
        } else {
            info.private |= access;
        }
    }

    private addAlias(aliasId: ts.Identifier, symbol: ts.Symbol): void {
        const aliasSym = this.checker.getSymbolAtLocation(aliasId)!;
        assert(!!aliasSym);
        multiMapAdd(this.localVariableAliases, symbol, aliasSym);
    }

    private addTypeAssignment(to: ts.Type, from: ts.Type): void {//maybe inline
        if (to === from) {
            return;
        }

        //If 'to' is a union, we need to assign to parts.
        if (isUnionOrIntersectionType(to)) {
            for (const t of to.types) { //name
                this.addTypeAssignment(t, from);
            }
            return;
        }

        //also, if assigning A[] to B[], we are also assigning A to B.
        //but I don't know how to do that...
        if (isTypeReference(to) && isTypeReference(from)) {
            //if (to.target === from.target) { //problem: if one is REadonlyARray and other is Array...
                if (to.typeArguments && from.typeArguments && to.typeArguments.length === from.typeArguments.length ) {
                    zip(to.typeArguments, from.typeArguments!, (argA, argB) => {
                        this.addTypeAssignment(argA, argB); //uh, variance...
                    });
                }
            //}
        }

        //should have a *set* of assignments, not an array (for perf)
        multiMapAdd(this.typeAssignmentsSourceToTarget, from, to);
        multiMapAdd(this.typeAssignmentsTargetToSource, to, from);
    }

    private addCastToTypeNode(node: ts.TypeNode): void {
        //todo: just use isTypeReference()
        if (ts.isTypeReferenceNode(node)) {
            //also handle type arguments (e.g. cast to a T[] means a T is created via cast)
            if (node.typeArguments) for (const t of node.typeArguments) {
                this.addCastToTypeNode(t);
            }
        }
        this.addCastToType(this.checker.getTypeAtLocation(node));
    }

    private addCastToType(type: ts.Type): void {
        if (isUnionOrIntersectionType(type)) {
            //test -- for `type T = { a } & { b }` we treat both `a` and `b` as implicitly-created if there is a cast to `T`
            for (const t of type.types) {
                this.addCastToType(t);
            }
            return;
        }

        if (!type.symbol) {
            return;
        }

        if (this.castedToTypes.has(type.symbol)) {
            return;
        }
        this.castedToTypes.add(type.symbol);


        //test: also casts to all of its property types
        for (const prop of this.checker.getPropertiesOfType(type)) {
            //this.addCastToType(this.checker.getDeclaredTypeOfSymbol(prop)) ought to work,
            //but getDeclaredTypeOFSymbol returns 'any' for `readonly a: { readonly b: number}`
            for (const d of prop.declarations!) {
                if ((ts.isPropertyDeclaration(d) || ts.isPropertySignature(d)) && d.type) {
                    this.addCastToTypeNode(d.type);
                }
            }
        }
    }
}

function zip<T>(a: ReadonlyArray<T>, b: ReadonlyArray<T>, cb: (a: T, b: T) => void): void {
    assert(a.length === b.length);
    for (let i = 0; i < a.length; i++) {
        cb(a[i], b[i]);
    }
}

function isPublicAccess(
    node: ts.Identifier,
    symbol: ts.Symbol,
    currentFile: ts.SourceFile,
    currentClass: ts.ClassLikeDeclaration | undefined,
): boolean {
    for (const decl of symbol.declarations!) {
        const p = canBePrivate(decl); //name
        if (p === undefined) {
            continue;
        }
        if (ts.isSourceFile(p)) { //test: module augmentation used only in its containing source file, still needs export
            //If it's declarede in this file but we implicitly use its type, it's a public use.
            return p !== currentFile || isSymbolFlagSet(symbol, ts.SymbolFlags.Type) && isPublicTypeUse(node);
        }
        return p !== currentClass;
    }
    // For anything other than a class element or export, all uses are public.
    return true;
}

export function canBePrivate(dec: ts.Declaration): ts.SourceFile | ts.ClassLikeDeclaration | undefined { //name
    const decl = skipStuff(dec); //neater
    const parent = decl.parent!;
    if (isExported(decl)) {
        return ts.isSourceFile(parent) ? parent : undefined;
    } else if (ts.isClassElement(decl)) {
        return ts.isClassLike(parent) ? parent : undefined;
    } else if (ts.isParameterPropertyDeclaration(decl)) {
        const cls = parent.parent!;
        return ts.isClassLike(cls) ? cls : undefined;
    } else {
        return undefined;
    }
}

//name, doc
function skipStuff(decl: ts.Declaration) {
    if (ts.isVariableDeclaration(decl)) {
        const p = decl.parent!;
        if (ts.isVariableDeclarationList(p)) {
            const pp = p.parent!;
            if (ts.isVariableStatement(pp)) {
                return pp;
            }
        }
    }
    return decl;
}

function isPublicTypeUse(node: ts.Identifier) {
    const parent = node.parent!;
    // The original declaration isn't a public use.
    return !(isUsageTrackedDeclaration(parent) && parent.name === node) && isPublicTypeUseWorker(parent);
}

function isPublicTypeUseWorker(node: ts.Node): boolean {
    switch (node.kind) {
        case ts.SyntaxKind.TypeAliasDeclaration:
        case ts.SyntaxKind.FunctionDeclaration:
        case ts.SyntaxKind.ClassDeclaration:
        case ts.SyntaxKind.ClassExpression:
        case ts.SyntaxKind.InterfaceDeclaration:
            return isExported(node);
        default:
            return (ts.isTypeNode(node) || ts.isClassElement(node) || ts.isTypeElement(node) || ts.isParameter(node))
                && isPublicTypeUseWorker(node.parent!);
    }
}

//!
function isExported(node: ts.Node): boolean {
    return hasModifier(node.modifiers, ts.SyntaxKind.ExportKeyword);
}

function createIfNotSet<K extends object, V>(map: Map<K, V> | WeakMap<K, V>, key: K, createValue: (key: K) => V): V {
    const already = map.get(key); //name
    if (already !== undefined) {
        return already;
    } else {
        const value = createValue(key);
        map.set(key, value);
        return value;
    }
}


function skipTransient(symbol: ts.Symbol): ts.Symbol {
    //todo: we shouldn't get these coming out of `getSymbolAtLocation`...
    if (isSymbolFlagSet(symbol, ts.SymbolFlags.Transient)) {
        const target = (symbol as any).target;
        return target === undefined ? symbol : target;
    } else {
        return symbol;
    }
}
