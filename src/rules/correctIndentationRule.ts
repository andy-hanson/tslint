/**
 * @license
 * Copyright 2013 Palantir Technologies, Inc.
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
import Char from "typescript-char";

import * as Lint from "../index";

export class Rule extends Lint.Rules.AbstractRule {
    /* tslint:disable:object-literal-sort-keys */
    public static metadata: Lint.IRuleMetadata = {
        ruleName: "correct-indentation",
        description: "",
        type: "style",
        typescriptOnly: false,
    };
    /* tslint:enable:object-literal-sort-keys */

    public static FAILURE_STRING(expectedIndent: number, actualIndent: number) {
        return `Expected indent to be ${expectedIndent}, actual: ${actualIndent}`;
    }

    public apply(sourceFile: ts.SourceFile): Lint.RuleFailure[] {
        return this.applyWithWalker(new Walker(sourceFile, this.getOptions()));
    }
}

class Walker extends Lint.RuleWalker {
    private indent = 0;
    private lineStarts: number[];
    private lineStartIndex = 0;
    private text: string;

    private nextLineStart() {
        return this.lineStarts[this.lineStartIndex];
    }

    constructor(sourceFile: ts.SourceFile, options: Lint.IOptions) {
        super(sourceFile, options)
        this.text = sourceFile.getFullText();
        this.lineStarts = getLineStarts(this.text);
        //Add a dummy extra one
        if (this.lineStarts[this.lineStarts.length - 1] !== this.text.length)
            this.lineStarts.push(this.text.length);
    }

    public visitNode(node: ts.Node) {
        if (node.kind === ts.SyntaxKind.SourceFile) {
            super.visitNode(node);
            return;
        }

        if (this.nextLineStart() > node.getStart()) {
            if (this.nextLineStart() <= node.getEnd()) {//optimization: ensure that there is a newline somewhere in there.
                super.visitNode(node);
            }
            return;
        }

        //node.pos is the "full" start, including comments. Since comemnts may be on the previous line, we really want getStart().
        const start = node.getStart();

        //handle multiple blank lines with no nodes
        while (this.lineStarts[this.lineStartIndex + 1] <= start) {
            this.lineStartIndex++;
        }

        const foo = () => {
            //console.log(node.getText());
            //Then node is on the next line and we must check it!
            //this.checkFirstNodeOnLine(node);
            const indent = start - this.nextLineStart();
            if (indent !== this.indent) {
                //console.log("FAIL AT:", ts.SyntaxKind[node.kind]);
                if (node.kind !== ts.SyntaxKind.EndOfFileToken) //kludge
                    this.addFailureAt(start, 1, Rule.FAILURE_STRING(this.indent, indent));
            }
            this.lineStartIndex++;

            this.indent++;
            super.visitNode(node);
            this.indent--;
        }

        if (node.kind === ts.SyntaxKind.Block && !isBlockStatementParent(node.parent!)) {
            // For e.g. `if\n{`, dont' require it to be indented.
            this.indent--;
            foo();
            this.indent++;
        } else {
            foo();
        }
    }
}

function isBlockStatementParent(node: ts.Node) {
    switch (node.kind) {
        //Test *all* of these!
        case ts.SyntaxKind.Block:
        case ts.SyntaxKind.ModuleBlock:
        case ts.SyntaxKind.SourceFile:
            return true;
        default:
            return false;
    }
}

// Returns array of positions where lines start.
function getLineStarts(text: string): number[] {
    const starts: number[] = [];
    // First character is considered a line start.
    starts.push(0);
    for (let i = 0; i < text.length; i++) {
        if (text.charCodeAt(i) === Char.LineFeed) {
            //Line start is the character *after* the newline
            starts.push(i + 1);
        }
    }
    return starts;
}




/*function nodeShouldIncreaseIndent(node: ts.Node): boolean {
    switch (node.kind) {
        //I better have tests for all of these...
        case ts.SyntaxKind.ForInStatement:
        case ts.SyntaxKind.ForOfStatement:
        case ts.SyntaxKind.ForStatement:
        case ts.SyntaxKind.IfStatement:
        case ts.SyntaxKind.DoStatement:
        case ts.SyntaxKind.WhileStatement:
        case ts.SyntaxKind.ModuleDeclaration:
        case ts.SyntaxKind.EnumDeclaration:
        case ts.SyntaxKind.ClassDeclaration:
        case ts.SyntaxKind.ObjectLiteralExpression:
        case ts.SyntaxKind.TypeLiteral:
        //case ts.SyntaxKind.MethodDeclaration:
        case ts.SyntaxKind.InterfaceDeclaration:
        case ts.SyntaxKind.MethodSignature:
        case ts.SyntaxKind.FunctionExpression:
        case ts.SyntaxKind.Block:
        case ts.SyntaxKind.CaseBlock:
            return true;
        default:
            return false;
    }
}*/

/*
//TODO: support spaces too
//Returns 'undefined' if this is not at the start of a line.
function countIndents(text: string, startIndex: number): number | undefined {
    let indent = 0;
    //count backwards
    for (let idx = startIndex - 1; ; idx--) {
        if (idx < 0) {
            break;
        }
        const ch = text.charCodeAt(idx);
        if (ch === Char.LineFeed) {
            break;
        }
        else if (ch === Char.Tab) {
            indent++;
        }
        else {
            return undefined;
        }
    }
    return indent;
}
*/
