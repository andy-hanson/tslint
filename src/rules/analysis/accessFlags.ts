import assert = require("assert");
import * as ts from "typescript";
import { isSymbolFlagSet } from '../..';
import { isAssignmentKind } from 'tsutils';

export class SymbolInfo {
    private = AccessFlags.None;
    public = AccessFlags.None;

    //kill
    show() { //tslint:disable-line no-unused-anything
        return JSON.stringify({ public: showAccessFlags(this.public), private: showAccessFlags(this.private) })
    }

    //todo: detect created-but-never-read

    everUsedAsMutableCollection(): boolean {
        return hasAccessFlag(this.private, AccessFlags.ReadWithMutableType) || hasAccessFlag(this.public, AccessFlags.ReadWithMutableType);
    }

    everCreatedOrWritten(): boolean {
        return hasAccessFlag(this.private, AccessFlags.Create | AccessFlags.Write)
            || hasAccessFlag(this.public, AccessFlags.Create | AccessFlags.Write);
    }

    everWritten(): boolean {
        return hasAccessFlag(this.private, AccessFlags.Write) || hasAccessFlag(this.public, AccessFlags.Write);
    }

    everUsedPublicly(): boolean {
        return this.public !== AccessFlags.None;
    }

    everRead(): boolean {
        return hasAccessFlag(this.private, AccessFlags.ReadEitherWay) || hasAccessFlag(this.public, AccessFlags.ReadEitherWay);
    }
}
function hasAccessFlag(a: AccessFlags, b: AccessFlags): boolean {
    return !!(a & b);
}

export const enum AccessFlags {
    None = 0,
    /** Only reads from a variable. */
    ReadReadonly = 2 ** 0,
    /** Reads from the variable, but may mutate it. */
    ReadWithMutableType = 2 ** 1,
    /** Only writes to a variable without using the result. E.g.: `x++;`. */
    Write = 2 ** 2, //If this is a method, this will be ignored.
    /** Creates it */
    Create = 2 ** 3,

    ReadEitherWay = ReadReadonly | ReadWithMutableType,
}

function showAccessFlags(f: AccessFlags) {
    const s = [];
    if (f & AccessFlags.ReadReadonly) s.push("ReadReadonly");
    if (f & AccessFlags.ReadWithMutableType) s.push("ReadWithMutableType");
    if (f & AccessFlags.Write) s.push("Write");
    if (f & AccessFlags.Create) s.push("Create");
    return s.length === 0 ? "None" : s.join();
}

export function accessFlags(
    node: ts.Identifier,
    symbol: ts.Symbol,
    checker: ts.TypeChecker,
    addAlias: (id: ts.Identifier) => void,
    addTypeAssignment: (to: ts.Type, from: ts.Type) => void,
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
        //if (isTested(parent) && parent.name === node) {
        //    return AccessFlags.Create;
        //}

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
                return node === name
                    // Recurse to parent to see how the property is being used.
                    ? this.work(parent as ts.PropertyAccessExpression, symbol, shouldAddAlias)
                    // If the method name is e.g. "push" we aren't *writing* to the symbol, but we are writing to its *content*.
                    : mutatingMethodNames.has(name.text) ? AccessFlags.ReadWithMutableType : AccessFlags.ReadReadonly;
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

            case ts.SyntaxKind.FunctionExpression:
                assert(node === (parent as ts.FunctionExpression).name);
                return AccessFlags.Create; // Doesn't matter, we won't analyze a function expression anyway.

            case ts.SyntaxKind.PostfixUnaryExpression:
            case ts.SyntaxKind.PrefixUnaryExpression:
                const { operator } = parent as ts.PrefixUnaryExpression | ts.PostfixUnaryExpression;
                return operator === ts.SyntaxKind.PlusPlusToken || operator === ts.SyntaxKind.MinusMinusToken
                    ? writeOrReadWrite(node, symbol)
                    : AccessFlags.ReadReadonly;

            case ts.SyntaxKind.ExpressionStatement:
                //test:
                //function f() { return 0; }
                //f(); //did not use the return value
                return AccessFlags.ReadReadonly;//AccessFlags.None;

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
                const { initializer, name, type } = parent as ts.VariableDeclaration;
                if (node === name) {
                    if (type && initializer) {
                        this.addTypeAssignment(this.checker.getTypeFromTypeNode(type), this.checker.getTypeAtLocation(initializer));
                    }
                    return AccessFlags.Create;
                } else {
                    assert(node === initializer);
                    if (ts.isIdentifier(name) && shouldAddAlias) {
                        this.addAlias(name);
                    }
                    // There may be no contextual type, in which case we consider this an immutable use for now.
                    // But we will track uses of the alias and handle that at the end.
                    return this.fromContext(node);
                }
            }

            case ts.SyntaxKind.BinaryExpression: {
                //note we are looking for mutations of the *value*, and *assigning* is ok.
                //e.g. `let x: ReadonlyArray<number>; x = [];` is not a mutable use.
                const { left, operatorToken, right } = parent as ts.BinaryExpression;
                if (operatorToken.kind === ts.SyntaxKind.EqualsToken) {
                    if (node === left) {
                        this.addTypeAssignment(
                            this.checker.getTypeAtLocation(left),
                            this.checker.getTypeAtLocation(right)); //test
                        // `x = ...` is a write.
                        return AccessFlags.Write;
                    } else {
                        // `... = x` means we are assigning this to something else,
                        // and need the contextual type to know if it's used as a readonly collection.
                        // When assigning to an existing variable there should always be a contextual type, so no need to track an alias.
                        return this.fromContext(node)
                    }
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
                if (node === name) {
                    return initializer ? AccessFlags.Create : AccessFlags.None;
                }
                else {
                    assert(node === initializer);
                    return this.fromContext(node);
                }
            }

            case ts.SyntaxKind.ExportSpecifier: {
                const { name, propertyName } = parent as ts.ExportSpecifier;
                //todo: add an alias
                if (propertyName === undefined) {
                    return AccessFlags.Create | AccessFlags.ReadReadonly; //todo; better -- one symbol is created, another read
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
                return AccessFlags.Create;
            //type uses don't mutate obvs
            case ts.SyntaxKind.QualifiedName:
            case ts.SyntaxKind.TypeQuery:
                return AccessFlags.ReadReadonly;

            case ts.SyntaxKind.TypePredicate://join
                return AccessFlags.ReadReadonly;

            case ts.SyntaxKind.ElementAccessExpression: {
                const { expression, argumentExpression } = parent as ts.ElementAccessExpression;
                if (node === argumentExpression) {
                    return AccessFlags.ReadReadonly;
                }
                else {
                    assert(node === expression);
                    //don't add alias for the parent, and symbol shouldn't matter
                    return this.work(parent as ts.ElementAccessExpression, undefined, /*shouldAddAlias*/ false) & AccessFlags.Write
                        ? AccessFlags.ReadWithMutableType
                        : AccessFlags.ReadReadonly;
                }
            }

            case ts.SyntaxKind.ConditionalExpression:
                return node === (parent as ts.ConditionalExpression).condition ? AccessFlags.ReadReadonly : this.fromContext(node);

            case ts.SyntaxKind.Parameter: {//initializer
                const { name, initializer } = parent as ts.ParameterDeclaration;
                if (node === name) {
                    return AccessFlags.Create;
                }
                else {
                    assert(node === initializer);
                    return this.fromContext(node);
                }
            }

                //these expressions may expose it mutably depending on the contextual type
            case ts.SyntaxKind.ArrayLiteralExpression:
            case ts.SyntaxKind.ReturnStatement:
            case ts.SyntaxKind.NewExpression:
            case ts.SyntaxKind.ArrowFunction: //return value
                return this.fromContext(node);

            case ts.SyntaxKind.PropertyAssignment:
                return node === (parent as ts.PropertyAssignment).name ? AccessFlags.Create : this.fromContext(node);

            case ts.SyntaxKind.ShorthandPropertyAssignment:
                //If this is the property symbol, this creates it. Otherwise this is reading a local variable.
                return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.Create : this.fromContext(node);

            case ts.SyntaxKind.ThrowStatement:
                return AccessFlags.ReadWithMutableType; //be pessimistic since we don't have typed exceptions

            case ts.SyntaxKind.MethodDeclaration:
                return AccessFlags.Create;
            case ts.SyntaxKind.GetAccessor:
            case ts.SyntaxKind.SetAccessor:
                return AccessFlags.Create;
            case ts.SyntaxKind.PropertyAssignment:
                return (parent as ts.PropertyAssignment).name === node ? AccessFlags.Create : AccessFlags.ReadReadonly;
            case ts.SyntaxKind.BindingElement:
                return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.ReadReadonly : AccessFlags.Create;

            default:
                throw new Error(`TODO: handle ${ts.SyntaxKind[parent.kind]}, ${parent.getText()}, node: ${ts.SyntaxKind[node.kind]} ${node.getText()}`)
        }
    }

    private fromContext(node: ts.Expression): AccessFlags {
        // In all other locations: if provided to a context with a readonly type, not a mutable use.
        // `function f(): ReadonlyArray<number> { return x; }` uses `x` readonly, `funciton f(): number[]` does not.
        const contextualType = this.checker.getContextualType(node); //uncast
        if (!contextualType) {
            // If there's no contextual type, be pessimistic.
            return AccessFlags.ReadWithMutableType;
        }
        this.addTypeAssignment(/*to*/ contextualType, /*from*/ this.checker.getTypeAtLocation(node));
        return !this.checker.typeToString(contextualType).startsWith("Readonly")
            ? AccessFlags.ReadWithMutableType
            : AccessFlags.ReadReadonly;
    }
}

function writeOrReadWrite(node: ts.Node, symbol: ts.Symbol | undefined): AccessFlags { //name
    const parent = node.parent!;
    // If grandparent is not an ExpressionStatement, this is used as an expression in addition to having a side effect.
    const shouldRead = parent.parent!.kind !== ts.SyntaxKind.ExpressionStatement;
    //test -- mutation in ctr is readonly
    const flags = symbol !== undefined && isInOwnConstructor(node, symbol) ? AccessFlags.Create : AccessFlags.Write;
    return shouldRead ? flags | AccessFlags.ReadReadonly : flags;
}

const mutatingMethodNames: ReadonlySet<string> = new Set([
    // Array
    "copyWithin",
    "pop",
    "push",
    "shift",
    "sort",
    "splice",
    "unshift",
    // Set / Map
    "add",
    "clear",
    "delete",
    "set",
])

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
