import assert = require("assert");
import * as ts from "typescript";
import { isTested } from '.';
import { isSymbolFlagSet } from '../..';

export class SymbolInfo {
    private = AccessFlags.None;
    public = AccessFlags.None;

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

//symbol optional so can call in an expression context - shorthandpropertyassignment won't occur in that case
//TODO: make accessFlagsWorker that takes no symbol and two entry points
//todo: combine with the other huge switch?
export function accessFlags(
    node: ts.Identifier,
    symbol: ts.Symbol,
    checker: ts.TypeChecker,
    addAlias: (id: ts.Identifier) => void,
): AccessFlags {
    return accessFlagsWorker(node, symbol, checker, addAlias);
}

function accessFlagsWorker(
    node: ts.Expression,
    symbol: ts.Symbol | undefined,
    checker: ts.TypeChecker,
    addAlias: ((id: ts.Identifier) => void) | undefined,
): AccessFlags {
    const parent = node.parent!;
    if (isTested(parent) && parent.name === node) {
        return AccessFlags.Create;
    }

    switch (parent.kind) {
        case ts.SyntaxKind.AsExpression:
        case ts.SyntaxKind.TypeAssertionExpression:
        case ts.SyntaxKind.NonNullExpression:
        case ts.SyntaxKind.ParenthesizedExpression:
            type T = ts.AsExpression | ts.TypeAssertion | ts.NonNullExpression | ts.ParenthesizedExpression;
            return accessFlagsWorker(parent as T, symbol, checker, addAlias);

        case ts.SyntaxKind.PropertyAccessExpression: {
            const { expression, name } = parent as ts.PropertyAccessExpression;
            if (name === node) {//preferconditional
                return accessFlagsWorker(parent as ts.PropertyAccessExpression, symbol, checker, addAlias);
            } else {
                assert(expression === node);
                return isMutatingMethodName(name.text) ? AccessFlags.ReadWithMutableType : AccessFlags.ReadReadonly;
            }
        }

        case ts.SyntaxKind.CallExpression: {
            const { expression } = parent as ts.CallExpression;
            if (node === expression) {//preferconditional
                return symbol && isSymbolFlagSet(symbol, ts.SymbolFlags.Method | ts.SymbolFlags.Function)
                    //for fn, we analyze the return type to see if it's used mutably
                    ? accessFlagsWorker(parent as ts.CallExpression, symbol, checker, addAlias)
                    : AccessFlags.ReadReadonly;
            } else {
                return fromContext();
            }
        }

        case ts.SyntaxKind.FunctionExpression:
            assert(node === (parent as ts.FunctionExpression).name);
            return AccessFlags.Create; //doesn't matter, we won't analyze this later anyway

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
        case ts.SyntaxKind.TaggedTemplateExpression:
        case ts.SyntaxKind.TemplateSpan:
        case ts.SyntaxKind.SwitchStatement: //the thing being switched on
        case ts.SyntaxKind.SpreadElement:
        case ts.SyntaxKind.TypeOfExpression:
            return AccessFlags.ReadReadonly;

        case ts.SyntaxKind.VariableDeclaration: {
            const { initializer, name } = parent as ts.VariableDeclaration;
            if (ts.isIdentifier(name)) {
                assert(initializer === node);
                addAlias && addAlias(name);
                return AccessFlags.ReadReadonly; //for now -- alias may be mutated though
            }
            else {
                return fromContext(); //?
            }
        }

        case ts.SyntaxKind.BinaryExpression: {
            //note we are looking for mutations of the *value*, and *assigning* is ok.
            //e.g. `let x: ReadonlyArray<number>; x = [];` is not a mutable use.
            const { left, operatorToken } = parent as ts.BinaryExpression;
            if (operatorToken.kind !== ts.SyntaxKind.EqualsToken) {
                return node === left && isAssignmentOperator(operatorToken.kind) ? writeOrReadWrite() : AccessFlags.ReadReadonly;
            }
            else if (node === left) {
                return AccessFlags.Write;
            }
            else {
                //Might be assigned to a mutable variable (test)
                //let x: string[]; x = y; //y can't be readonlyarray
                return fromContext();
            }
        }

        case ts.SyntaxKind.DeleteExpression:
            return AccessFlags.Write; //test: delete x[3] means x isn't readonlyarray

        case ts.SyntaxKind.PropertyDeclaration: {
            const { name, initializer } = parent as ts.PropertyDeclaration;
            if (node === name) {
                return AccessFlags.Create;
            }
            else {
                assert(node === initializer);
                return fromContext();
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
        case ts.SyntaxKind.NamespaceImport:
        case ts.SyntaxKind.EnumDeclaration:
        case ts.SyntaxKind.ImportSpecifier:
        case ts.SyntaxKind.ImportClause:
        case ts.SyntaxKind.ImportEqualsDeclaration:
            return AccessFlags.Create;
        //type uses don't mutate obvs
        case ts.SyntaxKind.QualifiedName:
        case ts.SyntaxKind.TypePredicate:
        case ts.SyntaxKind.TypeQuery:
            return AccessFlags.ReadReadonly;

        case ts.SyntaxKind.ElementAccessExpression: {
            const { expression, argumentExpression } = parent as ts.ElementAccessExpression;
            if (node === argumentExpression) {
                return AccessFlags.ReadReadonly;
            }
            else {
                assert(node === expression);
                return accessFlagsWorker(parent as ts.ElementAccessExpression, undefined, checker, undefined) & AccessFlags.Write
                    ? AccessFlags.ReadWithMutableType
                    : AccessFlags.ReadReadonly;
            }
        }

        case ts.SyntaxKind.ConditionalExpression:
            return node === (parent as ts.ConditionalExpression).condition ? AccessFlags.ReadReadonly : fromContext();

        case ts.SyntaxKind.Parameter: {//initializer
            const { name, initializer } = parent as ts.ParameterDeclaration;
            if (node === name) {
                return AccessFlags.Create;
            }
            else {
                assert(node === initializer);
                return fromContext();
            }
        }

            //these expressions may expose it mutably depending on the contextual type
        case ts.SyntaxKind.ArrayLiteralExpression:
        case ts.SyntaxKind.ReturnStatement:
        case ts.SyntaxKind.NewExpression:
        case ts.SyntaxKind.ArrowFunction: //return value
            return fromContext();

        case ts.SyntaxKind.PropertyAssignment:
        case ts.SyntaxKind.ShorthandPropertyAssignment:
            return AccessFlags.ReadWithMutableType;//TODO: only if the property being assigned is mutable type

        case ts.SyntaxKind.ThrowStatement:
            return AccessFlags.ReadWithMutableType; //be pessimistic since we don't have typed exceptions

        case ts.SyntaxKind.PropertyDeclaration:
            return createIf((parent as ts.PropertyDeclaration).initializer !== undefined);
        case ts.SyntaxKind.MethodDeclaration:
            return createIf((parent as ts.MethodDeclaration).body !== undefined);
        case ts.SyntaxKind.GetAccessor:
        case ts.SyntaxKind.SetAccessor:
            return AccessFlags.Create;
        case ts.SyntaxKind.PropertyAssignment:
            return (parent as ts.PropertyAssignment).name === node ? AccessFlags.Create : AccessFlags.ReadReadonly;
        case ts.SyntaxKind.ShorthandPropertyAssignment:
            //If this is the property symbol, this creates it. Otherwise this is reading a local variable.
            return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.Create : AccessFlags.ReadReadonly;
        case ts.SyntaxKind.BindingElement:
            return symbol!.flags & ts.SymbolFlags.Property ? AccessFlags.ReadReadonly : AccessFlags.Create;
        case ts.SyntaxKind.MethodDeclaration:
            return AccessFlags.Create;
        case ts.SyntaxKind.PostfixUnaryExpression:
        case ts.SyntaxKind.PrefixUnaryExpression:
            const { operator } = parent as ts.PrefixUnaryExpression | ts.PostfixUnaryExpression;
            return operator === ts.SyntaxKind.PlusPlusToken || operator === ts.SyntaxKind.MinusMinusToken
                ? writeOrReadWrite()
                : AccessFlags.ReadReadonly;

        default:
            throw new Error(`TODO: handle ${ts.SyntaxKind[parent.kind]}, ${parent.getText()}, node: ${ts.SyntaxKind[node.kind]} ${node.getText()}`)
    }

    //unclosure?
    function fromContext(): AccessFlags {
        // In all other locations: if provided to a context with a readonly type, not a mutable use.
        // `function f(): ReadonlyArray<number> { return x; }` uses `x` readonly, `funciton f(): number[]` does not.
        const contextualType = checker.getContextualType(node as ts.Expression); //uncast
        //If no contextual type, be pessimistic
        return !contextualType || !checker.typeToString(contextualType).startsWith("Readonly")
            ? AccessFlags.ReadWithMutableType
            : AccessFlags.ReadReadonly;
    }

    function createIf(b: boolean): AccessFlags {
        return b ? AccessFlags.Create : AccessFlags.None;
    }

    function writeOrReadWrite(): AccessFlags { //name
        // If grandparent is not an ExpressionStatement, this is used as an expression in addition to having a side effect.
        const shouldRead = !parent.parent || parent!.parent!.kind !== ts.SyntaxKind.ExpressionStatement;
        const flags = symbol && isInOwnConstructor(node, symbol) ? AccessFlags.Create : AccessFlags.Write;
        return shouldRead ? flags | AccessFlags.ReadReadonly : flags;
    }
}

//-> set
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
