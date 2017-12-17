declare function use(...args: any[]): void;

// Detects implicit export of types -- So 'C' must be exported here.
export class C {}
export function f() { return new C(); }

// Used, but no need to export
export const x = 0;
             ~ [noexport]
use(x);

[noexport]: Analysis found no uses in other modules; this should not be exported.
