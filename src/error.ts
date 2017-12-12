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

const shownWarnings = new Set<string>();

/**
 * Generic error typing for EcmaScript errors
 * Define `Error` here to avoid using `Error` from @types/node.
 * Using the `node` version causes a compilation error when this code is used as an npm library if @types/node is not already imported.
 */
export declare class Error {
    // tslint:disable-next-line no-unused-anything (TODO: remove?)
    public name?: string;
    public readonly message: string;
    public readonly stack?: string;
    constructor(message?: string);
}

/**
 * Used to exit the program and display a friendly message without the callstack.
 */
export class FatalError extends Error {
    private static readonly NAME = "FatalError";
    constructor(
        public readonly message: string,
        // tslint:disable-next-line no-unused-anything (TODO: remove this?)
        public readonly innerError?: Error,
    ) {
        super(message);
        this.name = FatalError.NAME;

        // Fix prototype chain for target ES5
        Object.setPrototypeOf(this, FatalError.prototype);
    }
}

export function isError(possibleError: any): possibleError is Error {
    return possibleError != undefined && (possibleError as Error).message !== undefined;
}

export function showWarningOnce(message: string) {
    if (!shownWarnings.has(message)) {
        console.warn(message);
        shownWarnings.add(message);
    }
}
