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

import assert = require("assert");
import { hasModifier, isSymbolFlagSet, isTypeFlagSet } from "tsutils";
import * as ts from "typescript";

import { getEqualsKind } from "../..";
import { skipAlias } from "../noUnnecessaryQualifierRule";
import { AccessFlags, accessFlags, SymbolInfo } from "./accessFlags";
import { multiMapAdd } from '../../utils';

const infoCache = new WeakMap<ts.Program, AnalysisResult>();
export function getInfo(program: ts.Program): AnalysisResult {
    return createIfNotSet(infoCache, program, () => getInfoWorker(program));
}

function getInfoWorker(program: ts.Program): AnalysisResult {
    const analyzer = new Analyzer(program.getTypeChecker());
    for (const file of program.getSourceFiles()) {
        if (file.fileName.includes("lib")) continue;//kill
        analyzer.analyze(file, file, undefined);
    }
    return analyzer.finish();
}

export type Tested = ts.NamedDeclaration & { readonly name: ts.Identifier };
export function isTested(node: ts.Node): node is Tested {
    switch (node.kind) {
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.PropertySignature:
        case ts.SyntaxKind.MethodSignature:
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
        case ts.SyntaxKind.ModuleDeclaration:
            type T = ts.PropertyDeclaration | ts.PropertySignature | ts.MethodSignature | ts.GetAccessorDeclaration | ts.SetAccessorDeclaration | ts.ModuleDeclaration;
            return ts.isIdentifier((node as T).name);
        case ts.SyntaxKind.MethodDeclaration:
            const parent = node.parent!;
            return !ts.isObjectLiteralExpression(parent) && ts.isIdentifier((node as ts.MethodDeclaration).name);
        //test: we detect unused interface, enum, type
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


/*export type Tested =
    (ElementOfClassOrInterface | ts.VariableDeclaration | ts.ParameterDeclaration | ts.FunctionDeclaration)
    & { readonly name: ts.Identifier }; // tslint:disable-line no-unused-anything (https://github.com/Microsoft/TypeScript/pull/20609)
export function isTested(node: ts.Node): node is Tested { //what does this mean???
    return (isElementOfClassOrInterface(node)
        || ts.isVariableDeclaration(node)
        || ts.isParameter(node)
        || ts.isFunctionDeclaration(node))
        && node.name !== undefined && ts.isIdentifier(node.name);
}*/

export interface AnalysisResult {
    readonly symbolInfos: ReadonlyMap<ts.Symbol, SymbolInfo>;
    readonly enumMembers: ReadonlyMap<ts.Symbol, EnumAccessFlags>;
}

class Analyzer {
    constructor(private readonly checker: ts.TypeChecker) {}
    private readonly symbolInfos = new Map<ts.Symbol, SymbolInfo>(); //name
    private readonly enumMembers = new Map<ts.Symbol, EnumAccessFlags>();
    private readonly localVariableAliases = new Map<ts.Symbol, ts.Symbol[]>();

    finish(): AnalysisResult {
        //todo: repeat until no changes (test)
        this.localVariableAliases.forEach((aliases, sym) => {
            const originalInfo = this.symbolInfos.get(sym)!;
            for (const alias of aliases) {
                //todo: might be a private alias...
                const aliasInfo = this.symbolInfos.get(alias)!;
                if (aliasInfo.everUsedAsMutableCollection()) {//todo: public/private difference
                    originalInfo.private |= AccessFlags.ReadWithMutableType;
                    originalInfo.public |= AccessFlags.ReadWithMutableType;
                }
            }
        });

        return { symbolInfos: this.symbolInfos, enumMembers: this.enumMembers };
    }

    analyze(node: ts.Node, currentFile: ts.SourceFile, currentClass: ts.ClassLikeDeclaration | undefined) {
        switch (node.kind) {
            case ts.SyntaxKind.Identifier: {
                const sym = this.checker.getSymbolAtLocation(node);
                if (sym !== undefined) {
                    const symbol = skipTransient(skipAlias(sym, this.checker));
                    if (isSymbolFlagSet(symbol, ts.SymbolFlags.EnumMember)) {
                        this.trackEnumMemberUse(node as ts.Identifier, symbol);
                    } else {
                        this.trackSymbolUse(node as ts.Identifier, symbol, currentFile, currentClass);
                    }
                }
                break;
            }
            case ts.SyntaxKind.ClassDeclaration:
            case ts.SyntaxKind.ClassExpression:
                node.forEachChild(child => this.analyze(child, currentFile, node as ts.ClassLikeDeclaration));
                break;
            default:
                node.forEachChild(child => this.analyze(child, currentFile, currentClass));
        }
    }

    private trackEnumMemberUse(node: ts.Identifier, symbol: ts.Symbol) {
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
        const bindingSymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, this.checker);
        if (bindingSymbol !== undefined) {
            this.trackUseOfEachRootSymbol(node, bindingSymbol, currentFile, currentClass);
        }
        else {
            const objectLiteral = getContainingObjectLiteralElement(node);
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
        for (const root of this.checker.getRootSymbols(symbol)) {
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
        const access = accessFlags(node, symbol, this.checker, aliasId => this.addAlias(aliasId, symbol));
        if (isPublicAccess(symbol, currentFile, currentClass)) {
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
}

function isPublicAccess(symbol: ts.Symbol, currentFile: ts.SourceFile, currentClass: ts.ClassLikeDeclaration | undefined) {
    for (const decl of symbol.declarations!) {
        if (hasModifier(decl.modifiers, ts.SyntaxKind.ExportKeyword)) {
            const parent = decl.parent!;
            if (ts.isSourceFile(parent)) {
                return parent !== currentFile;
            }
        }
        if (ts.isClassElement(decl)) {
            const declaringClass = decl.parent!;
            if (ts.isClassLike(declaringClass)) {
                return declaringClass !== currentClass;
            }
        }
    }
    // For anything other than a class element or export, all uses are public.
    return true;
}

//this is unused, why don't we catch it
export type PropertyDeclarationLike = ts.PropertyDeclaration | ts.ParameterDeclaration | ts.PropertySignature;

function createIfNotSet<K extends object, V>(map: Map<K, V> | WeakMap<K, V>, key: K, createValue: () => V): V {
    const already = map.get(key); //name
    if (already !== undefined) {
        return already;
    } else {
        const value = createValue();
        map.set(key, value);
        return value;
    }
}

//mv to accessflags.ts
export const enum EnumAccessFlags {
    None = 0,
    Tested = 2 ** 0,
    UsedInExpression = 2 ** 1,
}
export function hasEnumAccessFlag(a: EnumAccessFlags, b: EnumAccessFlags): boolean {
    return (a & b) !== EnumAccessFlags.None;
}

function accessFlagsForEnumAccess(n: ts.Identifier): EnumAccessFlags {
    const parent0 = n.parent!;
    if (ts.isEnumMember(parent0)) {
        return EnumAccessFlags.None;
    }

    if (!ts.isPropertyAccessExpression(parent0)) {
        //may be a binary expression if used inside the enum itself
        assert(ts.isQualifiedName(parent0) || ts.isBinaryExpression(parent0));
        return EnumAccessFlags.None; //used as a type, or used inside the enum itself
    }

    assert(ts.isPropertyAccessExpression(parent0));
    const parent = parent0.parent!;
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
    /*switch (parent.kind) {
        case ts.SyntaxKind.BinaryExpression: {
            const { operatorToken } = parent as ts.BinaryExpression;
            return isAssignmentOperator(operatorToken.kind) ? EnumAccessFlags.Tested : EnumAccessFlags.UsedInExpression;
        }
        //initializer
        case ts.SyntaxKind.PropertyAssignment:
        case ts.SyntaxKind.Parameter:
        case ts.SyntaxKind.PropertyDeclaration:
        case ts.SyntaxKind.VariableDeclaration:
            return EnumAccessFlags.UsedInExpression;
        default:
            return EnumAccessFlags.Tested;
    }*/
}


//function skipThings(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Symbol { //name
//    return skipTransient(skipPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, checker));
//}

function skipTransient(symbol: ts.Symbol): ts.Symbol {
    //todo: we shouldn't get these coming out of `getSymbolAtLocation`...
    return isSymbolFlagSet(symbol, ts.SymbolFlags.Transient) ? (symbol as any).target || symbol : symbol;
}





//taken from findALlReferences.ts
//TODO: should be a method on checker
function getObjectBindingElementWithoutPropertyName(symbol: ts.Symbol): ts.BindingElement | undefined {
    const bindingElement = symbol.declarations && symbol.declarations.find(ts.isBindingElement);
    if (bindingElement &&
        bindingElement.parent!.kind === ts.SyntaxKind.ObjectBindingPattern &&
        !bindingElement.propertyName) {
        return bindingElement;
    }
    return undefined; //neater
}
function getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Symbol | undefined {
    const bindingElement = getObjectBindingElementWithoutPropertyName(symbol);
    if (!bindingElement) return undefined;

    const typeOfPattern = checker.getTypeAtLocation(bindingElement.parent!);
    const propSymbol = typeOfPattern && checker.getPropertyOfType(typeOfPattern, (<ts.Identifier>bindingElement.name).text);
    if (propSymbol && isSymbolFlagSet(propSymbol, ts.SymbolFlags.Accessor)) {
        // See GH#16922
        assert(isSymbolFlagSet(propSymbol, ts.SymbolFlags.Transient));
        return (propSymbol as any/*ts.TransientSymbol*/).target;
    }
    return propSymbol;
}
function getContainingObjectLiteralElement(node: ts.Node): ts.ObjectLiteralElement | undefined {
    switch (node.kind) {
        case ts.SyntaxKind.StringLiteral:
        case ts.SyntaxKind.NumericLiteral:
            if (node.parent!.kind === ts.SyntaxKind.ComputedPropertyName) {
                return isObjectLiteralElement(node.parent!.parent!) ? node.parent!.parent as ts.ObjectLiteralElement : undefined;
            }
        // falls through
        case ts.SyntaxKind.Identifier:
            return isObjectLiteralElement(node.parent!) &&
                (node.parent!.parent!.kind === ts.SyntaxKind.ObjectLiteralExpression || node.parent!.parent!.kind === ts.SyntaxKind.JsxAttributes) &&
                (<ts.ObjectLiteralElement>node.parent).name === node ? node.parent as ts.ObjectLiteralElement : undefined;
    }
    return undefined;
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
    }
    return false;
}
/** Gets all symbols for one property. Does not get symbols for every property. */
function getPropertySymbolsFromContextualType(node: ts.ObjectLiteralElement, checker: ts.TypeChecker): ReadonlyArray<ts.Symbol> {
    const objectLiteral = <ts.ObjectLiteralExpression>node.parent; //todo: update typedef so don't need cast
    const contextualType = checker.getContextualType(objectLiteral);
    const name = getNameFromObjectLiteralElement(node);
    if (name && contextualType) {
        const result: ts.Symbol[] = [];
        const symbol = contextualType.getProperty(name);
        if (symbol) {
            result.push(symbol);
        }

        if (isTypeFlagSet(contextualType, ts.TypeFlags.Union)) {
            for (const t of (<ts.UnionType>contextualType).types) {
                const symbol = t.getProperty(name);
                if (symbol) {
                    result.push(symbol);
                }
            }
        }
        return result;
    }
    return [];
}
function getNameFromObjectLiteralElement(node: ts.ObjectLiteralElement): string | undefined {
    const name = node.name!;
    if (name.kind === ts.SyntaxKind.ComputedPropertyName) {
        const nameExpression = (<ts.ComputedPropertyName>node.name).expression;
        // treat computed property names where expression is string/numeric literal as just string/numeric literal
        if (isStringOrNumericLiteral(nameExpression)) {
            return (<ts.LiteralExpression>nameExpression).text;
        }
        return undefined;
    }
    return getTextOfIdentifierOrLiteral(name);
}
function getTextOfIdentifierOrLiteral(node: ts.Identifier | ts.LiteralLikeNode) {
    if (node.kind === ts.SyntaxKind.Identifier) {
        return node.text;
    }
    if (node.kind === ts.SyntaxKind.StringLiteral ||
        node.kind === ts.SyntaxKind.NoSubstitutionTemplateLiteral || //todo: add this to ts version
        node.kind === ts.SyntaxKind.NumericLiteral) {
        return (node).text;
    }
    throw new Error("");//!
}
function isStringOrNumericLiteral(node: ts.Node): node is ts.StringLiteral | ts.NumericLiteral {
    const kind = node.kind;
    return kind === ts.SyntaxKind.StringLiteral
        || kind === ts.SyntaxKind.NumericLiteral;
}
