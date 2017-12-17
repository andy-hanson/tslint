declare function use(...args: any[]): void;

// Detects implicit export of types -- So 'C' must be exported here.
export class C {}
export function f() { return new C(); }

// Value in `typeof` is also implicitly exported
export const s = "s";
export type T = typeof s;

// Used, but no need to export
export const x = 0;
             ~ [noexport]
use(x);

[noexport]: Analysis found no uses in other modules; this should not be exported.
