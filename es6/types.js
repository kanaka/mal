// Symbols
class Sym {
    constructor(name) { this.name = name; }
    toString() { return this.name; }
    name() { return this.name; }
}
export function _symbol(name) { return new Sym(name); }
export function _symbol_Q(obj) { return obj instanceof Sym; }
