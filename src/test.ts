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

import chalk from "chalk";
import * as diff from "diff";
import * as fs from "fs";
import * as glob from "glob";
import * as path from "path";
import * as semver from "semver";
import * as ts from "typescript";

import { Replacement } from "./language/rule/rule";
import * as Linter from "./linter";
import { Logger } from "./runner";
import { denormalizeWinPath, mapDefined, readBufferWithDetectedEncoding, flatMap } from "./utils";
import { LintError } from "./verify/lintError";
import * as parse from "./verify/parse";
import { ILinterOptions } from '.';

const MARKUP_FILE_EXTENSION = ".lint"; //d'oh, can't do imports any more!!!
const FIXES_FILE_EXTENSION = ".fix";

export interface TestOutput {
    readonly skipped: false;
    // tslint:disable-next-line no-unused-anything (TODO: use this!)
    readonly errorsFromLinter: ReadonlyArray<LintError>;
    // tslint:disable-next-line no-unused-anything (TODO: use this!)
    readonly errorsFromMarkup: ReadonlyArray<LintError>;
    readonly fixesFromLinter: string;
    readonly fixesFromMarkup: string;
    readonly markupFromLinter: string;
    readonly markupFromMarkup: string;
}

export interface SkippedTest {
    readonly skipped: true;
    readonly requirement: string;
}

export interface TestResult {
    // tslint:disable-next-line no-unused-anything (TODO: use this!)
    readonly directory: string;
    readonly results: {
        [fileName: string]: TestOutput | SkippedTest;
    };
}

export function runTests(patterns: ReadonlyArray<string>, rulesDirectory?: string | string[]): ReadonlyArray<TestResult> {
    const tslintFiles = flatMap(patterns, pattern =>
        glob.sync(path.basename(pattern) !== "tslint.json" ? path.join(pattern, "tslint.json") : pattern));
    return tslintFiles.map(file => runTest(path.dirname(file), rulesDirectory));
}

export function runTest(testDirectory: string, rulesDirectory?: string | string[]): TestResult {
    const filesToLint = [
        ...glob.sync(path.join(testDirectory, `**/*${MARKUP_FILE_EXTENSION}`)),
        //also allow plain '.ts' extension, so they can import each other!
        ...glob.sync(path.join(testDirectory, `**/*.ts`)),
    ];
    const tslintConfig = Linter.findConfiguration(path.join(testDirectory, "tslint.json"), "").results;
    const tsConfig = path.join(testDirectory, "tsconfig.json");
    let compilerOptions: ts.CompilerOptions = { allowJs: true };
    const hasConfig = fs.existsSync(tsConfig);
    if (hasConfig) {
        const {config, error} = ts.readConfigFile(tsConfig, ts.sys.readFile);
        if (error !== undefined) {
            throw new Error(JSON.stringify(error));
        }

        const parseConfigHost = {
            fileExists: fs.existsSync,
            readDirectory: ts.sys.readDirectory,
            readFile: (file: string) => fs.readFileSync(file, "utf8"),
            useCaseSensitiveFileNames: true,
        };
        compilerOptions = ts.parseJsonConfigFileContent(config, parseConfigHost, testDirectory).options;
    }
    const results: TestResult = { directory: testDirectory, results: {} };

    const isEncodingRule = path.basename(testDirectory) === "encoding";

    const mungledFiles = mapDefined(filesToLint, fileToLint => {
        const m = mungleFile(isEncodingRule, fileToLint);
        if ("skipped" in m) {
            results.results[fileToLint] = m;
            return undefined;
        }
        return m;
    });//name

    const program = hasConfig ? makeMeAProgram(compilerOptions, mungledFiles) : undefined;
    const lintOptions: ILinterOptions = {
        fix: false,
        formatter: "prose",
        formattersDirectory: "",
        rulesDirectory,
    };
    const linter = new Linter(lintOptions, program);
    for (const { fileToLint, fileCompileName, fileTextWithoutMarkup, errorsFromMarkup } of mungledFiles) {
        // Need to use the true path (ending in '.lint') for "encoding" rule so that it can read the file.
        linter.lint(isEncodingRule ? fileToLint : fileCompileName, fileTextWithoutMarkup, tslintConfig);
        //todo: want some way to ensure we don't drop failures...
        const failures = linter.getResult().failures.filter(f => f.getFileName() === fileCompileName);
        const errorsFromLinter = failures.map(failure => {
            const startLineAndCharacter = failure.getStartPosition().getLineAndCharacter();
            const endLineAndCharacter = failure.getEndPosition().getLineAndCharacter();

            return {
                endPos: {
                    col: endLineAndCharacter.character,
                    line: endLineAndCharacter.line,
                },
                message: failure.getFailure(),
                startPos: {
                    col: startLineAndCharacter.character,
                    line: startLineAndCharacter.line,
                },
            };
        });

        // test against fixed files
        let fixedFileText = "";
        let newFileText = "";
        try {
            const fixedFile = fileToLint.replace(/\.lint$/, "") + FIXES_FILE_EXTENSION;
            const stat = fs.statSync(fixedFile);
            if (stat.isFile()) {
                fixedFileText = fs.readFileSync(fixedFile, "utf8");
                const fixes = mapDefined(failures, (f) => f.getFix());
                newFileText = Replacement.applyFixes(fileTextWithoutMarkup, fixes);
            }
        } catch {
            fixedFileText = "";
            newFileText = "";
        }

        results.results[fileToLint] = {
            errorsFromLinter,
            errorsFromMarkup,
            fixesFromLinter: newFileText,
            fixesFromMarkup: fixedFileText,
            markupFromLinter: parse.createMarkupFromErrors(fileTextWithoutMarkup, errorsFromMarkup),
            markupFromMarkup: parse.createMarkupFromErrors(fileTextWithoutMarkup, errorsFromLinter),
            skipped: false,
        };
    }

    return results;
}

interface MungledFile {
    readonly fileToLint: string;
    readonly fileCompileName: string;
    readonly fileTextWithoutMarkup: string;
    readonly errorsFromMarkup: LintError[];
} //name
function mungleFile(isEncodingRule: boolean, fileToLint: string): MungledFile | SkippedTest {

    const fileCompileName = denormalizeWinPath(path.resolve(fileToLint.replace(/\.lint$/, "")));
    let fileText = isEncodingRule ? readBufferWithDetectedEncoding(fs.readFileSync(fileToLint)) : fs.readFileSync(fileToLint, "utf-8");
    const tsVersionRequirement = parse.getTypescriptVersionRequirement(fileText);
    if (tsVersionRequirement !== undefined) {
        // remove prerelease suffix when matching to allow testing with nightly builds
        if (!semver.satisfies(parse.getNormalizedTypescriptVersion(), tsVersionRequirement)) {
            return {
                requirement: tsVersionRequirement,
                skipped: true,
            };
        }
        // remove the first line from the file before continuing
        const lineBreak = fileText.search(/\n/);
        fileText = lineBreak === -1 ? "" : fileText.substr(lineBreak + 1);
    }
    fileText = parse.preprocessDirectives(fileText);
    const fileTextWithoutMarkup = parse.removeErrorMarkup(fileText);
    const errorsFromMarkup = parse.parseErrorsFromMarkup(fileText);
    return { fileToLint, fileCompileName, fileTextWithoutMarkup, errorsFromMarkup };
}

//todo: separate PR to make only one Program
function makeMeAProgram(compilerOptions: ts.CompilerOptions, mungledFiles: ReadonlyArray<MungledFile>): ts.Program {
    const compilerHost: ts.CompilerHost = {
        fileExists: (file) =>
            mungledFiles.some(m => m.fileCompileName === file) || fs.existsSync(file),
        getCanonicalFileName: (filename) => filename,
        getCurrentDirectory: () => process.cwd(),
        getDefaultLibFileName: () => ts.getDefaultLibFileName(compilerOptions),
        getDirectories: (dir) => fs.readdirSync(dir),
        getNewLine: () => "\n",
        getSourceFile(filenameToGet, target) {
            for (const m of mungledFiles) {
                if (denormalizeWinPath(filenameToGet) === m.fileCompileName) {
                    return ts.createSourceFile(filenameToGet, m.fileTextWithoutMarkup, target, true);
                }
            }
            if (path.basename(filenameToGet) === filenameToGet) {
                // resolve path of lib.xxx.d.ts
                filenameToGet = path.join(path.dirname(ts.getDefaultLibFilePath(compilerOptions)), filenameToGet);
            }
            const text = fs.readFileSync(filenameToGet, "utf8");
            return ts.createSourceFile(filenameToGet, text, target, true);
        },
        readFile: (x) => x,
        useCaseSensitiveFileNames: () => true,
        writeFile: () => null,
    };
    return ts.createProgram(mungledFiles.map(m => m.fileCompileName), compilerOptions, compilerHost);
}

export function consoleTestResultsHandler(testResults: ReadonlyArray<TestResult>, logger: Logger): boolean {
    let didAllTestsPass = true;

    for (const testResult of testResults) {
        if (!consoleTestResultHandler(testResult, logger)) {
            didAllTestsPass = false;
        }
    }

    return didAllTestsPass;
}

export function consoleTestResultHandler(testResult: TestResult, logger: Logger): boolean {
    // needed to get colors to show up when passing through Grunt
    (chalk as any).enabled = true;

    let didAllTestsPass = true;

    for (const fileName of Object.keys(testResult.results)) {
        const results = testResult.results[fileName];
        logger.log(`${fileName}:`);

        if (results.skipped) {
            logger.log(chalk.yellow(` Skipped, requires typescript ${results.requirement}\n`));
        } else {
            const markupDiffResults = diff.diffLines(results.markupFromMarkup, results.markupFromLinter);
            const fixesDiffResults = diff.diffLines(results.fixesFromLinter, results.fixesFromMarkup);
            const didMarkupTestPass = !markupDiffResults.some((hunk) => hunk.added === true || hunk.removed === true);
            const didFixesTestPass = !fixesDiffResults.some((hunk) => hunk.added === true || hunk.removed === true);

            if (didMarkupTestPass && didFixesTestPass) {
                logger.log(chalk.green(" Passed\n"));
            } else {
                logger.log(chalk.red(" Failed!\n"));
                didAllTestsPass = false;
                if (!didMarkupTestPass) {
                    displayDiffResults(markupDiffResults, MARKUP_FILE_EXTENSION, logger);
                }
                if (!didFixesTestPass) {
                    displayDiffResults(fixesDiffResults, FIXES_FILE_EXTENSION, logger);
                }
            }
        }
    }

    return didAllTestsPass;
}

function displayDiffResults(diffResults: ReadonlyArray<diff.IDiffResult>, extension: string, logger: Logger) {
    logger.log(chalk.green(`Expected (from ${extension} file)\n`));
    logger.log(chalk.red("Actual (from TSLint)\n"));

    for (const diffResult of diffResults) {
        let color = chalk.grey;
        if (diffResult.added) {
            color = chalk.green.underline;
        } else if (diffResult.removed) {
            color = chalk.red.underline;
        }
        logger.log(color(diffResult.value));
    }
}
