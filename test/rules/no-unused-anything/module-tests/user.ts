import { f, g, h, T } from "a";
declare function use(...args: any[]): void;
use(f() as T);
use(g({}));
use(h());
