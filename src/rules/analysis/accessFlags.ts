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
import { isSymbolFlagSet } from '../..';
import { isAssignmentKind } from 'tsutils';
import { isReadonlyType } from '../noUnusedAnythingRule';

export class SymbolInfo {
    public private = AccessFlags.None;
    public public = AccessFlags.None;

    //todo: detect created-but-never-read

    //kill
    show() { //tslint:disable-line no-unused-anything
        return { private: showAccessFlags(this.private), public: showAccessFlags(this.public) }
    }


    everUsedAsMutableCollection(): boolean {
        return this.has(AccessFlags.MutateCollectionEitherWay);
    }

    everUsedForSideEffect(): boolean {
        return this.has(AccessFlags.SideEffect);
    }

    everCreatedOrWritten(): boolean {
        return this.has(AccessFlags.AnyCreate | AccessFlags.Write);
    }

    everWritten(): boolean {
        return this.has(AccessFlags.Write);
    }

    everUsedPublicly(): boolean {
        return this.public !== AccessFlags.None;
    }

    everRead(): boolean {
        return this.has(AccessFlags.ReadEitherWay);
    }

    mutatesCollection(): boolean {
        return this.has(AccessFlags.MutateCollection);
    }

    //private
    has(flag: AccessFlags): boolean {
        return hasAccessFlag(this.private | this.public, flag);
    }
}
function hasAccessFlag(a: AccessFlags, b: AccessFlags): boolean {
    return !!(a & b);
}



function showAccessFlags(f: AccessFlags) {//kill
    const s = [];
    if (f & AccessFlags.ReadReadonly) s.push("ReadReadonly");
    if (f & AccessFlags.ReadWithMutableType) s.push("ReadWithMutableType");
    if (f & AccessFlags.MutateCollection) s.push("MutateCollection");
    if (f & AccessFlags.Write) s.push("Write");
    if (f & AccessFlags.CreateFresh) s.push("CreateFresh");
    if (f & AccessFlags.CreateAlias) s.push("CreateAlias");
    if (f & AccessFlags.SideEffect) s.push("CreateSideEffect");
    return s.length === 0 ? "None" : s.join();
}

export const enum AccessFlags {
    None = 0,
    /** Only reads from a variable. */
    ReadReadonly = 2 ** 0,
    /** Assign this to a variable that's not a readonly collection. */
    ReadWithMutableType = 2 ** 1,
    /** Call a method like `push()` that's *purely* a setter; we will detect collections that are *only* pushed to. */
    MutateCollection = 2 ** 2,
    /** Only writes to a variable without using the result. E.g.: `x++;`. */
    Write = 2 ** 3, //If this is a method, this will be ignored.
    /** Creates it as in `x = []` */
    CreateFresh = 2 ** 4,
    /** Creates it as in `x = f()` (may be an alias) */
    CreateAlias = 2 ** 5,
    //Calling `f()` uses it for a side effect, but doesn't touch the return value.
    SideEffect = 2 ** 6,

    AnyCreate = CreateFresh | CreateAlias,
    ReadEitherWay = ReadReadonly | ReadWithMutableType,
    MutateCollectionEitherWay = ReadWithMutableType | MutateCollection,
}

export function accessFlags(
    node: ts.Identifier,
    symbol: ts.Symbol,
    checker: ts.TypeChecker,
    addAlias: (id: ts.Identifier) => void,
    addTypeAssignment: (to: ts.Type, from: ts.Type) => void,//kill?
): AccessFlags {
    return new AccessFlagsChecker(checker, addAlias, addTypeAssignment).work(node, symbol, true);
}
class AccessFlagsChecker {
    constructor(
        private readonly checker: ts.TypeChecker,
        //
        private readonly addAlias: (id: ts.Identifier) => void,
        //shouldn't be here because this is only called on identifiers, do in loop in index.ts
        private readonly addTypeAssignment: (to: ts.Type, from: ts.Type) => void,
    ) {}

    work(node: ts.Expression, symbol: ts.Symbol | undefined, shouldAddAlias: boolean): AccessFlags {
        const parent = node.parent!;
        switch (parent.kind) {
            case ts.SyntaxKind.AsExpression:
            case ts.SyntaxKind.TypeAssertionExpression:
            case ts.SyntaxKind.NonNullExpression:
            case ts.SyntaxKind.ParenthesizedExpression:
                //use addtypeassignment here?
                type T = ts.AsExpression | ts.TypeAssertion | ts.NonNullExpression | ts.ParenthesizedExpression;
                return this.work(parent as T, symbol, shouldAddAlias);

            case ts.SyntaxKind.PropertyAccessExpression: {
                const { name } = parent as ts.PropertyAccessExpression;
                if (node === name) {
                    // Recurse to parent to see how the property is being used.
                    return this.work(parent as ts.PropertyAccessExpression, symbol, shouldAddAlias);
                } else {
                    //If it's then used by a call expression, may be mutating a collection.
                    const gp = parent.parent!; //name
                    if (ts.isCallExpression(gp)) {
                        // If the method name is e.g. "push" we aren't *writing* to the symbol, but we are writing to its *content*.
                        const collectionMutateKind = getCollectionMutateKind(name.text);
                        switch (collectionMutateKind) {
                            case CollectionMutateKind.None:
                                return AccessFlags.ReadReadonly;
                            case CollectionMutateKind.ReturnCollection:
                                return this.work(gp, symbol, shouldAddAlias) | AccessFlags.MutateCollection;
                            case CollectionMutateKind.ReturnData:
                                //gets data in addition to mutating the collection, so recurse to see if that data is used.
                                return this.work(gp, symbol, shouldAddAlias) | AccessFlags.MutateCollection;
                            case CollectionMutateKind.ReturnNonUseful:
                                //returns the length of the collection,
                                //but if that's all you're getting, having the collection is stupid, so mark as readonly
                                return AccessFlags.MutateCollection;
                            default:
                                return assertNever(collectionMutateKind);
                        }
                        //mutatingMethodNames.has(name.text) ? AccessFlags.MutateCollection : AccessFlags.ReadReadonly;
                    } else {
                        return AccessFlags.ReadReadonly;
                    }
                }
            }

            case ts.SyntaxKind.CallExpression: {
                const { expression } = parent as ts.CallExpression;
                return node !== expression
                    ? this.fromContext(node)
                    : symbol && isSymbolFlagSet(symbol, ts.SymbolFlags.Method | ts.SymbolFlags.Function)
                        // For a callable, we analyze the return type to see if it's used mutably.
                        ? this.work(parent as ts.CallExpression, symbol, shouldAddAlias)
                        : AccessFlags.ReadReadonly;
            }
            case ts.SyntaxKind.NewExpression:
                return node !== (parent as ts.NewExpression).expression
                    ? this.fromContext(node)
                    : AccessFlags.ReadReadonly;

            case ts.SyntaxKind.FunctionExpression:
                assert(node === (parent as ts.FunctionExpression).name);
                return AccessFlags.None; // Doesn't matter, we won't analyze a function expression anyway.

            case ts.SyntaxKind.PostfixUnaryExpression:
            case ts.SyntaxKind.PrefixUnaryExpression:
                const { operator } = parent as ts.PrefixUnaryExpression | ts.PostfixUnaryExpression;
                return operator === ts.SyntaxKind.PlusPlusToken || operator === ts.SyntaxKind.MinusMinusToken
                    ? writeOrReadWrite(node, symbol)
                    : AccessFlags.ReadReadonly;

            case ts.SyntaxKind.ExpressionStatement:
                return AccessFlags.SideEffect;

            case ts.SyntaxKind.BindingElement:
            case ts.SyntaxKind.ComputedPropertyName:
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
            case ts.SyntaxKind.TaggedTemplateExpression:
            case ts.SyntaxKind.TemplateSpan:
            case ts.SyntaxKind.SwitchStatement: //the thing being switched on
            case ts.SyntaxKind.SpreadElement:
            case ts.SyntaxKind.TypeOfExpression:
                return AccessFlags.ReadReadonly;

            case ts.SyntaxKind.VariableDeclaration: {
                const { initializer, name, parent: gp } = parent as ts.VariableDeclaration; //name
                if (node === name) {
                    return ts.isVariableStatement(gp!.parent!) ? createFlag(initializer) : AccessFlags.CreateAlias;
                } else {
                    assert(node === initializer);
                    const addedAlias = ts.isIdentifier(name) && shouldAddAlias;
                    if (addedAlias) {
                        this.addAlias(name as ts.Identifier);
                    }
                    // There may be no contextual type, in which case we consider this an immutable use for now.
                    // But we will track uses of the alias and handle that at the end.
                    return this.fromContext(node, addedAlias);
                }
            }

            case ts.SyntaxKind.BinaryExpression: {
                //note we are looking for mutations of the *value*, and *assigning* is ok.
                //e.g. `let x: ReadonlyArray<number>; x = [];` is not a mutable use.
                const { left, operatorToken } = parent as ts.BinaryExpression;
                if (operatorToken.kind === ts.SyntaxKind.EqualsToken) {
                    return node === left
                        // `x = ...` is a write.
                        ? AccessFlags.Write
                        // `... = x` means we are assigning this to something else,
                        // and need the contextual type to know if it's used as a readonly collection.
                        // When assigning to an existing variable there should always be a contextual type, so no need to track an alias.
                        : this.fromContext(node);
                } else {
                    return node === left && isAssignmentKind(operatorToken.kind)
                        // `x += ...` is treated as a write, and also as a read if it appears in another expression as in `f(x += 1)`.
                        ? writeOrReadWrite(node, symbol)
                        // `... += x` is a pure read.
                        : AccessFlags.ReadReadonly;
                }
            }

            case ts.SyntaxKind.DeleteExpression:
                return AccessFlags.Write;

            case ts.SyntaxKind.PropertyDeclaration: {
                const { name, initializer } = parent as ts.PropertyDeclaration;
                return node === name ? createFlag(initializer) : this.fromContext(node);
            }

            case ts.SyntaxKind.ExportSpecifier: {
                const { name, propertyName } = parent as ts.ExportSpecifier;
                //todo: add an alias
                if (propertyName === undefined) {
                    return AccessFlags.CreateAlias | AccessFlags.ReadReadonly; //todo; better -- one symbol is created, another read
                }
                return node === name ? AccessFlags.ReadReadonly : AccessFlags.Write;
            }

            case ts.SyntaxKind.TypeReference:
                return AccessFlags.ReadReadonly;

            case ts.SyntaxKind.PropertySignature:
                return AccessFlags.None;

            case ts.SyntaxKind.TypeAliasDeclaration:
            case ts.SyntaxKind.ModuleDeclaration:
            case ts.SyntaxKind.InterfaceDeclaration:
            case ts.SyntaxKind.ExpressionWithTypeArguments:
            case ts.SyntaxKind.ClassDeclaration:
            case ts.SyntaxKind.ExportAssignment:
            case ts.SyntaxKind.NamespaceExportDeclaration:
            case ts.SyntaxKind.TypeParameter:
            case ts.SyntaxKind.NamespaceImport:
            case ts.SyntaxKind.EnumDeclaration:
            case ts.SyntaxKind.ImportSpecifier:
            case ts.SyntaxKind.ImportClause:
            case ts.SyntaxKind.ImportEqualsDeclaration:
            case ts.SyntaxKind.FunctionDeclaration:
            case ts.SyntaxKind.MethodSignature: //TODO: warn for optional method that's never created
                return AccessFlags.CreateFresh;
            //type uses don't mutate obvs
            case ts.SyntaxKind.QualifiedName:
            case ts.SyntaxKind.TypeQuery:
                return AccessFlags.ReadReadonly;

            case ts.SyntaxKind.TypePredicate://join
                return AccessFlags.ReadReadonly;

            case ts.SyntaxKind.ElementAccessExpression: {
                const { expression, argumentExpression } = parent as ts.ElementAccessExpression;
                if (node === argumentExpression) {
                    //used as the index
                    return AccessFlags.ReadReadonly;
                }
                else {
                    assert(node === expression);
                    //don't add alias for the parent, and symbol shouldn't matter
                    const parentFlags = this.work(parent as ts.ElementAccessExpression, undefined, /*shouldAddAlias*/ false);
                    let flags = AccessFlags.None;
                    if (parentFlags & AccessFlags.Write) {
                        //writes an element of the array (test)
                        flags |= AccessFlags.MutateCollection;
                    }
                    if (parentFlags & AccessFlags.ReadEitherWay) {
                        //reads an element of the array (test)
                        flags |= AccessFlags.ReadReadonly;
                    }
                    return flags;
                }
            }

            case ts.SyntaxKind.ConditionalExpression:
                return node === (parent as ts.ConditionalExpression).condition ? AccessFlags.ReadReadonly : this.fromContext(node);

            case ts.SyntaxKind.Parameter: {
                const { name, initializer } = parent as ts.ParameterDeclaration;
                if (node === name) {
                    return AccessFlags.AnyCreate;
                }
                else {
                    assert(node === initializer);
                    return this.fromContext(node);
                }
            }

                //these expressions may expose it mutably depending on the contextual type
            case ts.SyntaxKind.ArrayLiteralExpression:
            case ts.SyntaxKind.ReturnStatement:
            case ts.SyntaxKind.ArrowFunction: //return value
                return this.fromContext(node);

            case ts.SyntaxKind.ShorthandPropertyAssignment:
                //If this is the property symbol, this creates it. Otherwise this is reading a local variable.
                return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.CreateAlias : this.fromContext(node);

            case ts.SyntaxKind.ThrowStatement:
                return AccessFlags.ReadWithMutableType; //be pessimistic since we don't have typed exceptions

            case ts.SyntaxKind.MethodDeclaration:
                return AccessFlags.AnyCreate;
            case ts.SyntaxKind.GetAccessor:
            case ts.SyntaxKind.SetAccessor:
                return AccessFlags.AnyCreate;
            case ts.SyntaxKind.PropertyAssignment: {
                const { name, initializer } = parent as ts.PropertyAssignment;
                return name === node ? createFlag(initializer) : this.fromContext(initializer);
            }
            case ts.SyntaxKind.BindingElement:
                return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.ReadReadonly : AccessFlags.CreateAlias;

            default:
                throw new Error(`TODO: handle ${ts.SyntaxKind[parent.kind]}, ${parent.getText()}, node: ${ts.SyntaxKind[node.kind]} ${node.getText()}`)
        }
    }

    private fromContext(node: ts.Expression, addedAlias = false): AccessFlags {
        // In all other locations: if provided to a context with a readonly type, not a mutable use.
        // `function f(): ReadonlyArray<number> { return x; }` uses `x` readonly, `funciton f(): number[]` does not.
        const contextualType = this.checker.getContextualType(node); //uncast
        if (!contextualType) {
            // If there's no contextual type, be pessimistic.
            //but for variable declaration be optimistic because we just added an alias (test)
            return addedAlias ? AccessFlags.ReadReadonly : AccessFlags.ReadWithMutableType;
        }
        this.addTypeAssignment(/*to*/ contextualType, /*from*/ this.checker.getTypeAtLocation(node));
        return !isReadonlyType(contextualType) ? AccessFlags.ReadWithMutableType : AccessFlags.ReadReadonly;
    }
}


function createFlag(initializer: ts.Expression | undefined): AccessFlags {
    return initializer === undefined
        ? AccessFlags.None
        : isFreshCollection(initializer)
        ? AccessFlags.CreateFresh
        : AccessFlags.CreateAlias;
}
function isFreshCollection(initializer: ts.Expression): boolean {
    return ts.isArrayLiteralExpression(initializer) ||
        ts.isNewExpression(initializer) && isMutableCollectionTypeName(initializer.expression);
}

//kill
export function isMutableCollectionTypeName(node: ts.Node): node is ts.Identifier {
    return ts.isIdentifier(node) && isMutableCollectionTypeNameStr(node.text);
}
export function isMutableCollectionTypeNameStr(name: string): boolean {
    return mutableCollectionTypeNames.has(name);
}
const mutableCollectionTypeNames: ReadonlySet<string> = new Set(["Array", "Set", "Map"]); //name better, reuse?

function writeOrReadWrite(node: ts.Node, symbol: ts.Symbol | undefined): AccessFlags { //name
    const parent = node.parent!;
    // If grandparent is not an ExpressionStatement, this is used as an expression in addition to having a side effect.
    const shouldRead = parent.parent!.kind !== ts.SyntaxKind.ExpressionStatement;
    //test -- mutation in ctr is readonly
    const flags = symbol !== undefined && isInOwnConstructor(node, symbol) ? AccessFlags.AnyCreate : AccessFlags.Write;
    return shouldRead ? flags | AccessFlags.ReadReadonly : flags;
}

const enum CollectionMutateKind {
    None,
    ReturnCollection,
    ReturnData, //In addition to mutating the collection, returns info inside it
    ReturnNonUseful, //e.g. 'push' returns the len, Set.add returns undefined.
}
function getCollectionMutateKind(name: string): CollectionMutateKind {
    switch (name) {
        case "copyWithin":
        case "sort":
        case "add": //from Set
        case "set": //from Map
            return CollectionMutateKind.ReturnCollection;
        case "pop":
        case "shift":
        case "splice":
        case "delete": //from Set/Map, returns whether delete succeeded
            return CollectionMutateKind.ReturnData;
        case "push": //returns array length
        case "unshift": //ditto
        case "clear": //returns undefined
            return CollectionMutateKind.ReturnNonUseful;
        default:
            return CollectionMutateKind.None;
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

//function isAssignmentOperator(token: ts.SyntaxKind): boolean {
//    return token >= ts.SyntaxKind.FirstAssignment && token <= ts.SyntaxKind.LastAssignment;/
//}

//mv
export function multiMapAdd<K, V>(map: Map<K, V[]>, key: K, value: V): void {
    const values = map.get(key);
    if (values === undefined) {
        map.set(key, [value]);
    } else {
        values.push(value);
    }
}

//mv
function assertNever(value: never): never {
    throw new Error(value);
}
