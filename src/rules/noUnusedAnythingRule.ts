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
import { hasModifier, isSymbolFlagSet } from "tsutils";
import * as ts from "typescript";

import * as Lint from "..";
import { skipAlias } from "./noUnnecessaryQualifierRule";

//todo: check type declarations -- e.g. if an array is never pushed to, use a ReadonlyArray
//TODO: warning for things created but never read
//todo: test that we detect unused enum members
//todo: detect parameter of recursive fn only passed to another instance of the same fn
//TODO: error if optional property is never created/written to

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
        const info = getInfo(program);
        const checker = program.getTypeChecker();
        return this.applyWithWalker(new Walker(sourceFile, this.ruleName, parseOptions(this.getOptions()), info, checker));
    }
}

interface JsonOptions {
    readonly ignoreNames?: ReadonlyArray<string>;
}

function parseOptions(gotOptions: Lint.IOptions): Options {
    const obj0 = gotOptions.ruleArguments[0] as JsonOptions;
    const obj = obj0 === undefined ? {} : obj0;
    return {
        ignoreNames: obj.ignoreNames === undefined ? new Set() : new Set(obj.ignoreNames),
    }
}

interface Options {
    readonly ignoreNames: ReadonlySet<string>;
}

class Walker extends Lint.AbstractWalker<Options> {
    constructor(
        sourceFile: ts.SourceFile,
        ruleName: string,
        options: Options,
        private readonly info: Info,
        private readonly checker: ts.TypeChecker,
    ) {
        super(sourceFile, ruleName, options);
    }

    public walk(sourceFile: ts.SourceFile): void {
        const cb = (node: ts.Node) => {
            switch (node.kind) {
                case ts.SyntaxKind.PropertySignature:
                case ts.SyntaxKind.PropertyDeclaration:
                    this.handleProp(node as ts.PropertyDeclaration | ts.PropertySignature);
                    break;
                case ts.SyntaxKind.Parameter:
                    if (ts.isParameterPropertyDeclaration(node)) {
                        this.handleProp(node as ts.ParameterDeclaration);
                    } else {
                        //todo
                    }
            }
            node.forEachChild(cb);
        }
        sourceFile.forEachChild(cb);
    }

    handleProp(p: PropertyDeclarationLike): void {
        if (!ts.isIdentifier(p.name)) {
            return;
        }
        if (this.options.ignoreNames.has(p.name.text)) { //test
            return;
        }


        const sym = p.kind === ts.SyntaxKind.Parameter ? getThePropertySymbol(p, this.checker) : this.checker.getSymbolAtLocation(p.name)!;
        assert(!!sym);

        //console.log((sym as any).id, sym.declarations![0].getText());
        const x = this.info.properties.get(sym);
        //console.log(x); //kill
        if (x === undefined) {
            this.addFailureAtNode(p, "Property is unused.");
        }
        else {
            if (!x.everUsedPublicly() && !hasModifier(p.modifiers, ts.SyntaxKind.PrivateKeyword, ts.SyntaxKind.ProtectedKeyword)) {
                this.addFailureAtNode(p, "Property can be made private.")
            }
            if (!x.everMutated() && !hasModifier(p.modifiers, ts.SyntaxKind.ReadonlyKeyword)) {
                this.addFailureAtNode(p, "Property can be made readonly.");
            }
        }
    }
}

function getThePropertySymbol(p: ts.ParameterDeclaration, checker: ts.TypeChecker) { //name
    return checker.getSymbolsOfParameterPropertyDeclaration(p, (p.name as ts.Identifier).text)
        .find(s => isSymbolFlagSet(s, ts.SymbolFlags.Property))!;
}


const infoCache = new WeakMap<ts.Program, Info>();

function getInfo(program: ts.Program): Info {
    return createIfNotSet(infoCache, program, () => getInfoWorker(program));
}

function getInfoWorker(program: ts.Program): Info {
    const f = new Foo(program.getTypeChecker());
    for (const sf of program.getSourceFiles()) {
        f.analyze(sf);
    }
    return f.finish();
}

interface Info {
    readonly properties: ReadonlyMap<ts.Symbol, PropertyInfo>;
}

//name
class Foo {
    constructor(private readonly checker: ts.TypeChecker) {}
    private readonly properties = new Map<ts.Symbol, PropertyInfo>();

    finish(): Info {
        return { properties: this.properties };
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
                default:
                    node.forEachChild(x => cb(x, currentClass));//name
            }
        };
        cb(file, undefined);
    }

    trackSymbolUse(node: ts.Identifier, sym: ts.Symbol, currentClass: ts.ClassLikeDeclaration | undefined): void {
        const xx = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(sym, this.checker); //name
        if (xx) {
            sym = xx;
        }
        if (sym.flags & ts.SymbolFlags.Transient) {
            //todo: we shouldn't get these coming out of `getSymbolAtLocation`...
            sym = (sym as any).target || sym;
        }
        const o = getContainingObjectLiteralElement(node);
        if (o) {
            for (const x of getPropertySymbolsFromContextualType(o, this.checker)) {
                //todo: might actually be a method
                this.trackPropertyUse(node, x, currentClass);
            }
        }
        else if (isSymbolFlagSet(sym, ts.SymbolFlags.Property)) {
            if (isPropertyDeclarationLike(node.parent!)) {
                //this *is* the declaration.
                return;
            }

            //showFlags(sym);
            //const paramprop = sym.declarations.find(d => ts.isParameterPropertyDeclaration(d)) as ts.ParameterDeclaration | undefined;
            //if (paramprop) {
            //    const sym2 = getThePropertySymbol(paramprop, this.checker);
            //    //assert(sym2 !== sym); //why?
            //    sym = sym2;
            //}

            for (const s of this.checker.getRootSymbols(sym)) {
                this.trackPropertyUse(node, s, currentClass);
            }
        }

        //todo: handle other kinds of things (methods)
    }

    trackPropertyUse(node: ts.Identifier, sym: ts.Symbol, currentClass: ts.ClassLikeDeclaration | undefined) {//name
        if (!sym.declarations) {
            return; //how does this happen?
        }
        const info = createIfNotSet(this.properties, sym, () => new PropertyInfo());
        const newFlags = accessFlags(node, sym);
        const isPrivate = sym.declarations!.some(d => d.parent === currentClass);
        if (isPrivate) {
            info.private = info.private | newFlags;
        } else {
            info.public = info.public | newFlags;
        }
    }
}

//taken from findALlReferences.ts
//TODO: should be a method on checker
function getObjectBindingElementWithoutPropertyName(symbol: ts.Symbol): ts.BindingElement | undefined {
    const bindingElement = symbol.declarations && symbol.declarations.find(ts.isBindingElement);
    //console.log(symbol.declarations![0].getText());
    if (bindingElement &&
        bindingElement.parent!.kind === ts.SyntaxKind.ObjectBindingPattern &&
        !bindingElement.propertyName) {
        return bindingElement;
    }
    return undefined; //neater
}
function getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol: ts.Symbol, checker: ts.TypeChecker): ts.Symbol | undefined {
    const bindingElement = getObjectBindingElementWithoutPropertyName(symbol);
    //console.log("??");
    if (!bindingElement) return undefined;
    //console.log("??");

    const typeOfPattern = checker.getTypeAtLocation(bindingElement.parent!);
    const propSymbol = typeOfPattern && checker.getPropertyOfType(typeOfPattern, (<ts.Identifier>bindingElement.name).text);
    if (propSymbol && propSymbol.flags & ts.SymbolFlags.Accessor) {
        // See GH#16922
        assert(!!(propSymbol.flags & ts.SymbolFlags.Transient));
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
function getPropertySymbolsFromContextualType(node: ts.ObjectLiteralElement, checker: ts.TypeChecker): ts.Symbol[] {
    const objectLiteral = <ts.ObjectLiteralExpression>node.parent; //todo: update typedef so don't need cast
    const contextualType = checker.getContextualType(objectLiteral);
    const name = getNameFromObjectLiteralElement(node);
    if (name && contextualType) {
        const result: ts.Symbol[] = [];
        const symbol = contextualType.getProperty(name);
        if (symbol) {
            result.push(symbol);
        }

        if (contextualType.flags & ts.TypeFlags.Union) {
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

//!
export function showFlags(sym: ts.Symbol) {
    const all: string[] = [];
    for (const name in ts.SymbolFlags) {
        const val = ts.SymbolFlags[name];
        if (typeof val !== "number") continue;
        if (sym.flags & val) {
            all.push(name);
        }
    }
    console.log(all.join());
}

type PropertyDeclarationLike = ts.PropertyDeclaration | ts.ParameterDeclaration | ts.PropertySignature;
function isPropertyDeclarationLike(node: ts.Node): node is PropertyDeclarationLike {
    return ts.isPropertyDeclaration(node) || ts.isPropertySignature(node) || ts.isParameterPropertyDeclaration(node);
}

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

class PropertyInfo {
    private = AccessFlags.None;
    public = AccessFlags.None;

    //todo: detect created-but-never-read

    everMutated(): boolean {
        return hasAccessFlag(this.private, AccessFlags.Mutate) || hasAccessFlag(this.public, AccessFlags.Mutate);
    }

    everUsedPublicly(): boolean {
        return this.public !== AccessFlags.None;
    }
}
function hasAccessFlag(a: AccessFlags, b: AccessFlags): boolean {
    return !!(a & b);
}


//TODO: this is from TypeScript, make it public
const enum AccessFlags {
    None = 0,
    /** Only reads from a variable. */
    Read = 2 ** 0,
    /** Only writes to a variable without using the result. E.g.: `x++;`. */
    Mutate = 2 ** 1,
    /** Creates it */
    Create = 2 ** 2,
}
function accessFlags(node: ts.Node, symbol: ts.Symbol): AccessFlags {
    const parent = node.parent!;
    switch (parent.kind) {
        case ts.SyntaxKind.PropertyAssignment:
        case ts.SyntaxKind.ShorthandPropertyAssignment: //test
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

    function writeOrReadWrite(): AccessFlags { //name
        // If grandparent is not an ExpressionStatement, this is used as an expression in addition to having a side effect.
        const shouldRead = !parent.parent || parent!.parent!.kind !== ts.SyntaxKind.ExpressionStatement;
        const flags = isInOwnConstructor(node, symbol) ? AccessFlags.Create : AccessFlags.Mutate;
        return shouldRead ? flags | AccessFlags.Read : flags;
    }
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

