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
import { isSymbolFlagSet, isTypeFlagSet } from "tsutils";
import * as ts from "typescript";

import { skipAlias } from "../noUnnecessaryQualifierRule";
import { getEqualsKind } from '../..';

const infoCache = new WeakMap<ts.Program, Info>();

export function getInfo(program: ts.Program): Info {
    return createIfNotSet(infoCache, program, () => getInfoWorker(program));
}

function getInfoWorker(program: ts.Program): Info {
    const f = new Foo(program.getTypeChecker());
    for (const sf of program.getSourceFiles()) {
        f.analyze(sf);
    }
    return f.finish();
}

//name
export type Tested = (ElementOfClassOrInterface | ts.VariableDeclaration | ts.ParameterDeclaration | ts.FunctionDeclaration) & { readonly name: ts.Identifier }; //tslint:disable-line no-unused-anything
export function isTested(node: ts.Node): node is Tested {
    return (isElementOfClassOrInterface(node)
        || ts.isVariableDeclaration(node)
        || ts.isParameter(node)
        || ts.isFunctionDeclaration(node))
        && node.name !== undefined && ts.isIdentifier(node.name);
}

export interface Info {
    readonly properties: ReadonlyMap<ts.Symbol, SymbolInfo>;
    readonly enumMembers: ReadonlyMap<ts.Symbol, EnumAccessFlags>;
    readonly interfaceIsDirectlyCreated: ReadonlySet<ts.Symbol>;
    readonly symbolToTypeIsMutated: ReadonlySet<ts.Symbol>; //name
}

//name
class Foo {
    constructor(private readonly checker: ts.TypeChecker) {}
    private readonly properties = new Map<ts.Symbol, SymbolInfo>(); //name
    private readonly enumMembers = new Map<ts.Symbol, EnumAccessFlags>();
    private readonly interfaceIsDirectlyCreated = new Set<ts.Symbol>();
    private readonly symbolToTypeIsMutated = new Set<ts.Symbol>();
    //tracks when a property or return is assigned to a local variable.
    private readonly localVariableAliases = new Map<ts.Symbol, ts.Symbol[]>();

    finish(): Info {
        //todo: repeat until no changes
        this.localVariableAliases.forEach((aliases, sym) => {
            for (const alias of aliases) {
                if (this.symbolToTypeIsMutated.has(alias)) {
                    this.symbolToTypeIsMutated.add(sym);
                }
            }
        });

        return {
            properties: this.properties,
            enumMembers: this.enumMembers,
            interfaceIsDirectlyCreated: this.interfaceIsDirectlyCreated,
            symbolToTypeIsMutated: this.symbolToTypeIsMutated,
        };
    }

    analyze(file: ts.SourceFile): void {
        if (file.fileName.includes("lib")) return;//kill

        const cb = (node: ts.Node, currentClass: ts.ClassLikeDeclaration | undefined) => {
            switch (node.kind) {
                case ts.SyntaxKind.Identifier: {
                    const sym = this.checker.getSymbolAtLocation(node);
                    if (sym) {
                        let sym2 = skipAlias(sym, this.checker);
                        this.trackSymbolUse(node as ts.Identifier, sym2, currentClass);
                    }
                    break;
                }
                case ts.SyntaxKind.ClassDeclaration:
                case ts.SyntaxKind.ClassExpression:
                    node.forEachChild(x => cb(x, node as ts.ClassLikeDeclaration));
                    break;
                case ts.SyntaxKind.ObjectLiteralExpression: {
                    const type = this.checker.getContextualType(node as ts.ObjectLiteralExpression);
                    if (type && type.symbol) {
                        this.interfaceIsDirectlyCreated.add(type.symbol);
                    }
                }
                default:
                    node.forEachChild(x => cb(x, currentClass));//name
            }
        };
        cb(file, undefined);
    }

    private trackSymbolUse(node: ts.Identifier, symbol: ts.Symbol, currentClass: ts.ClassLikeDeclaration | undefined): void {
        if (isSymbolFlagSet(symbol, ts.SymbolFlags.EnumMember)) {
            this.trackEnumMemberUse(node, symbol);
        } else {
            this.fff(node, symbol, currentClass);
        }
    }

    private trackEnumMemberUse(node: ts.Identifier, symbol: ts.Symbol) {
        const prevFlags = this.enumMembers.get(symbol);
        const flags = accessFlagsForEnumAccess(node);
        this.enumMembers.set(symbol, flags | (prevFlags === undefined ? EnumAccessFlags.None : prevFlags));
    }

    private fff(node: ts.Identifier, sym: ts.Symbol, currentClass: ts.ClassLikeDeclaration | undefined) {
        const symbol = skipTransient(sym);

        const xx = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, this.checker);
        if (xx) {
            this.trackUseOfEachRootSymbol(node, symbol, currentClass);
            this.trackUseOfEachRootSymbol(node, xx, currentClass);
            return;
        }

        const o = getContainingObjectLiteralElement(node);
        if (o) {
            for (const x of getPropertySymbolsFromContextualType(o, this.checker)) {
                this.trackUseOfEachRootSymbol(node, x, currentClass);
            }
            //we're doing this both for the property and for the value referenced by shorthand
            //test: function f(x) return { x };
            const parent = node.parent!;
            if (ts.isShorthandPropertyAssignment(parent)) {
                const v = this.checker.getShorthandAssignmentValueSymbol(parent)!;
                this.trackUseOfEachRootSymbol(node, v, currentClass);
            }
            return;
        }

        this.trackUseOfEachRootSymbol(node, symbol, currentClass);
    }

    private trackUseOfEachRootSymbol(node: ts.Identifier, symbol: ts.Symbol, currentClass: ts.ClassLikeDeclaration | undefined) {
        for (const root of this.checker.getRootSymbols(symbol)) {
            this.trackUse(node, root, currentClass);
        }
    }

    private trackUse(node: ts.Identifier, symbol: ts.Symbol, currentClass: ts.ClassLikeDeclaration | undefined): void {//name
        if (symbol.declarations === undefined) {
            return;
        }

        const info = createIfNotSet(this.properties, symbol, () => new SymbolInfo()); //if not a property -- fine, whatever
        const newFlags = accessFlags(node, symbol);
        if (symbol.declarations.some(d => d.parent === currentClass)) {
            info.private = info.private | newFlags;
        } else {
            info.public = info.public | newFlags;
        }

        //track use of the type -- if this is array or map or set, see if we mutate it
        const x = isPossiblyMutated(node, symbol, this.checker, aliasId => {
            const aliasSym = this.checker.getSymbolAtLocation(aliasId)!;
            assert(!!aliasSym);
            multiMapAdd(this.localVariableAliases, symbol, aliasSym);
        }); //name
        if (x) {
            this.symbolToTypeIsMutated.add(symbol);
        }
    }
}

function multiMapAdd<K, V>(map: Map<K, V[]>, key: K, value: V): void {
    const values = map.get(key);
    if (values === undefined) {
        map.set(key, [value]);
    } else {
        values.push(value);
    }
}

function isPossiblyMutated(
    node: ts.Identifier,
    symbol: ts.Symbol,
    checker: ts.TypeChecker,
    addAlias: (id: ts.Identifier) => void
): boolean {
    const x = isSymbolFlagSet(symbol, ts.SymbolFlags.Method | ts.SymbolFlags.Function) ? getCall(node) : node;
    return x !== undefined && isPossiblyMutableUse(x, checker, addAlias);
}

function getCall(node: ts.Identifier) {
    const parent = node.parent!;
    const x = ts.isPropertyAccessExpression(parent) && parent.name === node ? parent.parent! : parent;
    return ts.isCallExpression(x) ? x : undefined;
}

function isPossiblyMutableUse(node: ts.Expression, checker: ts.TypeChecker, addAlias: (id: ts.Identifier) => void): boolean {
    //TODO: `x[0]` is immutable but `x[0] = 1` isn't
    const parent = node.parent!;
    if (isTested(parent) && parent.name === node) {
        return false;
    }
    switch (parent.kind) {
        case ts.SyntaxKind.AsExpression:
        case ts.SyntaxKind.TypeAssertionExpression:
        case ts.SyntaxKind.NonNullExpression:
        case ts.SyntaxKind.ParenthesizedExpression:
            return isPossiblyMutableUse(parent as ts.AsExpression | ts.TypeAssertion | ts.NonNullExpression | ts.ParenthesizedExpression, checker, addAlias);

        case ts.SyntaxKind.PropertyAccessExpression: {
            const { expression, name } = parent as ts.PropertyAccessExpression;
            if (name === node) {//prefer conditional
                //x.y.z.array
                return isPossiblyMutableUse(parent as ts.PropertyAccessExpression, checker, addAlias);
            } else {
                assert(expression === node);
                //Check that it's not a mutating method
                return isMutatingMethodName(name.text);
            }
        }
        case ts.SyntaxKind.ExpressionStatement:
        case ts.SyntaxKind.BindingElement:
        case ts.SyntaxKind.ComputedPropertyName:
        case ts.SyntaxKind.PrefixUnaryExpression:
        case ts.SyntaxKind.PostfixUnaryExpression:
        case ts.SyntaxKind.IfStatement:
        case ts.SyntaxKind.AwaitExpression:
        case ts.SyntaxKind.SpreadAssignment:
        case ts.SyntaxKind.ForInStatement:
        case ts.SyntaxKind.ForOfStatement:
        case ts.SyntaxKind.ForStatement:
        case ts.SyntaxKind.WhileStatement: //condition
        case ts.SyntaxKind.DoStatement:
        case ts.SyntaxKind.VoidExpression:
        case ts.SyntaxKind.CaseClause:
            return false;
        case ts.SyntaxKind.VariableDeclaration: {
            const { initializer, name } = parent as ts.VariableDeclaration;
            if (ts.isIdentifier(name)) {
                assert(initializer === node);
                addAlias(name);
                return false; //for now -- alias may be mutated though
            }
            return fromContext();
        }

        case ts.SyntaxKind.BinaryExpression: {
            //note we are looking for mutations of the *value*, and *assigning* is ok.
            //e.g. `let x: ReadonlyArray<number>; x = [];` is not a mutable use.
            const { left, operatorToken } = parent as ts.BinaryExpression;
            if (node === left || operatorToken.kind !== ts.SyntaxKind.EqualsToken) {
                return false;
            }
            //Assigning to something -- check that we're assigning to a readonly thing
            return fromContext();
        }

        case ts.SyntaxKind.PropertyDeclaration:
            return node === (parent as ts.PropertyDeclaration).initializer && fromContext();

        //creations don't mutate obvs
        case ts.SyntaxKind.PropertySignature:
        case ts.SyntaxKind.TypeAliasDeclaration:
        case ts.SyntaxKind.ModuleDeclaration:
        case ts.SyntaxKind.TypeReference:
        case ts.SyntaxKind.InterfaceDeclaration:
        case ts.SyntaxKind.ExpressionWithTypeArguments:
        case ts.SyntaxKind.ClassDeclaration:
        case ts.SyntaxKind.ExportAssignment:
        case ts.SyntaxKind.NamespaceExportDeclaration:
        case ts.SyntaxKind.TypeParameter:
        case ts.SyntaxKind.ExportSpecifier:
        case ts.SyntaxKind.NamespaceImport:
        case ts.SyntaxKind.TypeOfExpression:
        case ts.SyntaxKind.TemplateSpan:
        case ts.SyntaxKind.SpreadElement:
        case ts.SyntaxKind.SwitchStatement: //the thing being switched on
        case ts.SyntaxKind.EnumDeclaration:
        case ts.SyntaxKind.ImportSpecifier:
        case ts.SyntaxKind.ImportClause:
        case ts.SyntaxKind.ImportEqualsDeclaration:
            return false;

        //type uses don't mutate obvs
        case ts.SyntaxKind.QualifiedName:
        case ts.SyntaxKind.TypePredicate:
        case ts.SyntaxKind.TypeQuery:
            return false;

        case ts.SyntaxKind.ElementAccessExpression: {
            const { expression, argumentExpression } = parent as ts.ElementAccessExpression;
            if (node === argumentExpression) {
                return false;
            }
            assert(node === expression);
            return propertyIsMutated(parent as ts.ElementAccessExpression);
        }

        case ts.SyntaxKind.ConditionalExpression:
            return node !== (parent as ts.ConditionalExpression).condition && fromContext();

        //these expressions may expose it mutably depending on the contextual type
        case ts.SyntaxKind.ArrayLiteralExpression:
        case ts.SyntaxKind.ReturnStatement:
        case ts.SyntaxKind.NewExpression:
        case ts.SyntaxKind.CallExpression:
        case ts.SyntaxKind.ArrowFunction: //return value
        case ts.SyntaxKind.Parameter: //initializer
            return fromContext();

        case ts.SyntaxKind.PropertyAssignment:
        case ts.SyntaxKind.ShorthandPropertyAssignment:
            return true;//TODO: only if the property being assigned is mutable type

        case ts.SyntaxKind.ThrowStatement:
            return true; //be pessimistic since we don't have typed exceptions

        default:
            throw new Error(`TODO: handle ${ts.SyntaxKind[parent.kind]}, ${parent.getText()}`)
    }

    //unclosure?
    function fromContext(): boolean {
        // In all other locations: if provided to a context with a readonly type, not a mutable use.
        // `function f(): ReadonlyArray<number> { return x; }` uses `x` readonly, `funciton f(): number[]` does not.
        const contextualType = checker.getContextualType(node);
        //If no contextual type, be pessimistic
        return !contextualType || !checker.typeToString(contextualType).startsWith("Readonly");
    }
}

function isMutatingMethodName(name: string): boolean {
    switch (name) {
        // Array
        case "copyWithin":
        case "pop":
        case "push":
        case "shift":
        case "sort":
        case "splice":
        case "unshift":
        // Set / Map
        case "add":
        case "clear":
        case "delete":
        case "set":
            return true;
        default:
            return false;
    }
}


//TODO: handle non-identifier
export type ElementOfClassOrInterface =
    // tslint:disable no-unused-anything (This is a TypeScript bug. See https://github.com/Microsoft/TypeScript/pull/20609)
    (ts.ClassElement | ts.TypeElement) & {
        readonly parent: ts.ClassLikeDeclaration | ts.InterfaceDeclaration;
        readonly name: ts.Identifier;
    };
    // tslint:enable no-unused-anything
function isElementOfClassOrInterface(node: ts.Node): node is ElementOfClassOrInterface {
    return (ts.isClassElement(node) || ts.isTypeElement(node))
        && node.name !== undefined
        && ts.isIdentifier(node.name)
        && (ts.isClassLike(node.parent!) || ts.isInterfaceDeclaration(node.parent!));
}

//!
export function showFlags(symbol: ts.Symbol) {
    const all: string[] = [];
    for (const name in ts.SymbolFlags) {
        const val = ts.SymbolFlags[name];
        if (typeof val !== "number") continue;
        if (isSymbolFlagSet(symbol, val)) {
            all.push(name);
        }
    }
    console.log(all.join());
}

export type PropertyDeclarationLike = ts.PropertyDeclaration | ts.ParameterDeclaration | ts.PropertySignature;
//function isPropertyDeclarationLike(node: ts.Node): node is PropertyDeclarationLike {
//    return ts.isPropertyDeclaration(node) || ts.isPropertySignature(node) || ts.isParameterPropertyDeclaration(node);
//}

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

export class SymbolInfo {
    private = AccessFlags.None;
    public = AccessFlags.None;

    //todo: detect created-but-never-read

    everCreatedOrMutated(): boolean {
        return hasAccessFlag(this.private, AccessFlags.Create | AccessFlags.Mutate)
            || hasAccessFlag(this.public, AccessFlags.Create | AccessFlags.Mutate);
    }

    everMutated(): boolean {
        return hasAccessFlag(this.private, AccessFlags.Mutate) || hasAccessFlag(this.public, AccessFlags.Mutate);
    }

    everUsedPublicly(): boolean {
        return this.public !== AccessFlags.None;
    }

    everRead(): boolean {
        return hasAccessFlag(this.private, AccessFlags.Read) || hasAccessFlag(this.public, AccessFlags.Read);
    }
}
function hasAccessFlag(a: AccessFlags, b: AccessFlags): boolean {
    return !!(a & b);
}


export const enum AccessFlags {
    None = 0,
    /** Only reads from a variable. */
    Read = 2 ** 0,
    /** Only writes to a variable without using the result. E.g.: `x++;`. */
    Mutate = 2 ** 1, //If this is a method, this will be ignored.
    /** Creates it */
    Create = 2 ** 2,
}

function propertyIsMutated(node: ts.PropertyAccessExpression | ts.ElementAccessExpression): boolean {
    return !!(accessFlags(node) & AccessFlags.Mutate);
}

//symbol optional so can call in an expression context - shorthandpropertyassignment won't occur in that case
//TODO: make accessFlagsWorker that takes no symbol and two entry points
function accessFlags(node: ts.Node, symbol?: ts.Symbol): AccessFlags {
    const parent = node.parent!;
    if (ts.isTypeElement(parent)) {
        return AccessFlags.None;
    }

    switch (parent.kind) {
        case ts.SyntaxKind.Parameter:
            return AccessFlags.Create;
        case ts.SyntaxKind.PropertyDeclaration:
            return createIf((parent as ts.PropertyDeclaration).initializer !== undefined);
        case ts.SyntaxKind.MethodDeclaration:
            return createIf((parent as ts.MethodDeclaration).body !== undefined);
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
            return AccessFlags.Create;
        case ts.SyntaxKind.PropertyAssignment:
            return (parent as ts.PropertyAssignment).name === node ? AccessFlags.Create : AccessFlags.Read;
        case ts.SyntaxKind.ShorthandPropertyAssignment:
            //If this is the property symbol, this creates it. Otherwise this is reading a local variable.
            return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.Create : AccessFlags.Read;
        case ts.SyntaxKind.BindingElement:
            return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.Read : AccessFlags.Create;
        case ts.SyntaxKind.MethodDeclaration:
            return AccessFlags.Create;
        case ts.SyntaxKind.PostfixUnaryExpression:
        case ts.SyntaxKind.PrefixUnaryExpression:
            const { operator } = parent as ts.PrefixUnaryExpression | ts.PostfixUnaryExpression;
            return operator === ts.SyntaxKind.PlusPlusToken || operator === ts.SyntaxKind.MinusMinusToken
                ? writeOrReadWrite()
                : AccessFlags.Read;
        case ts.SyntaxKind.BinaryExpression:
            const { left, operatorToken } = parent as ts.BinaryExpression;
            return left === node && isAssignmentOperator(operatorToken.kind) ? writeOrReadWrite() : AccessFlags.Read;
        case ts.SyntaxKind.PropertyAccessExpression:
            return (parent as ts.PropertyAccessExpression).name !== node ? AccessFlags.Read : accessFlags(parent, symbol);
        default:
            return AccessFlags.Read;
    }

    function createIf(b: boolean): AccessFlags {
        return b ? AccessFlags.Create : AccessFlags.None;
    }

    function writeOrReadWrite(): AccessFlags { //name
        // If grandparent is not an ExpressionStatement, this is used as an expression in addition to having a side effect.
        const shouldRead = !parent.parent || parent!.parent!.kind !== ts.SyntaxKind.ExpressionStatement;
        const flags = symbol && isInOwnConstructor(node, symbol) ? AccessFlags.Create : AccessFlags.Mutate;
        return shouldRead ? flags | AccessFlags.Read : flags;
    }
}

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
        assert(ts.isQualifiedName(parent0));
        assert((ts as any).isPartOfTypeNode(parent0));
        return EnumAccessFlags.None;
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

function isInOwnConstructor(node: ts.Node, symbol: ts.Symbol): boolean { //test
    const decl = symbol.valueDeclaration;
    if (!decl || !ts.isPropertyDeclaration(decl) && !ts.isClassLike(decl.parent!)) { //todo: handle parameter property too
        return false;
    }
    const ownConstructor = (decl.parent as ts.ClassLikeDeclaration).members.find(m => ts.isConstructorDeclaration(m) && !!m.body);

    while (true) {
        const parent = node.parent;
        if (parent === undefined) {
            return false;
        }
        if (ts.isFunctionLike(parent)) {
            return parent === ownConstructor;
        }
        node = parent;
    }
}

function isAssignmentOperator(token: ts.SyntaxKind): boolean {
    return token >= ts.SyntaxKind.FirstAssignment && token <= ts.SyntaxKind.LastAssignment;
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
