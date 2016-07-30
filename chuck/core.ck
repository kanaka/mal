public class Core
{
    static string names[];
    static MalSubr ns[];
}

["+", "-", "*", "/",
 "list", "list?", "empty?", "count",
 "=", "<", "<=", ">", ">=",
 "pr-str", "str", "prn", "println",
 "read-string", "slurp",
 "atom", "atom?", "deref", "reset!", "swap!",
 "cons", "concat"] @=> Core.names;
MalSubr ns[0] @=> Core.ns;

new MalAdd @=> Core.ns["+"];
new MalSub @=> Core.ns["-"];
new MalMul @=> Core.ns["*"];
new MalDiv @=> Core.ns["/"];

new MalListify @=> Core.ns["list"];
new MalIsList @=> Core.ns["list?"];
new MalIsEmpty @=> Core.ns["empty?"];
new MalCount @=> Core.ns["count"];

new MalEqual @=> Core.ns["="];
new MalLess @=> Core.ns["<"];
new MalLessEqual @=> Core.ns["<="];
new MalGreater @=> Core.ns[">"];
new MalGreaterEqual @=> Core.ns[">="];

new MalPrStr @=> Core.ns["pr-str"];
new MalStr @=> Core.ns["str"];
new MalPrn @=> Core.ns["prn"];
new MalPrintln @=> Core.ns["println"];

new MalReadStr @=> Core.ns["read-string"];
new MalSlurp @=> Core.ns["slurp"];

new MalAtomify @=> Core.ns["atom"];
new MalIsAtom @=> Core.ns["atom?"];
new MalDeref @=> Core.ns["deref"];
new MalDoReset @=> Core.ns["reset!"];
new MalDoSwap @=> Core.ns["swap!"];

new MalCons @=> Core.ns["cons"];
new MalConcat @=> Core.ns["concat"];
