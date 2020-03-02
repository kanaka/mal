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
 "cons", "concat",
 "nth", "first", "rest",
 "throw",
 "apply", "map",
 "nil?", "true?", "false?", "number?", "symbol?", "keyword?", "vector?", "map?",
 "symbol", "keyword", "vector", "hash-map",
 "assoc", "dissoc", "get", "contains?", "keys", "vals",
 "sequential?", "fn?", "macro?",
 "readline", "meta", "with-meta",
 "time-ms", "conj", "string?", "seq"] @=> Core.names;
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

new MalNth @=> Core.ns["nth"];
new MalFirst @=> Core.ns["first"];
new MalRest @=> Core.ns["rest"];

new MalThrow @=> Core.ns["throw"];

new MalApply @=> Core.ns["apply"];
new MalMap @=> Core.ns["map"];

new MalIsNil @=> Core.ns["nil?"];
new MalIsTrue @=> Core.ns["true?"];
new MalIsFalse @=> Core.ns["false?"];
new MalIsNumber @=> Core.ns["number?"];
new MalIsSymbol @=> Core.ns["symbol?"];
new MalIsKeyword @=> Core.ns["keyword?"];
new MalIsVector @=> Core.ns["vector?"];
new MalIsHashMap @=> Core.ns["map?"];

new MalSymbolify @=> Core.ns["symbol"];
new MalKeywordify @=> Core.ns["keyword"];
new MalVectorify @=> Core.ns["vector"];
new MalHashMapify @=> Core.ns["hash-map"];

new MalAssoc @=> Core.ns["assoc"];
new MalDissoc @=> Core.ns["dissoc"];
new MalGet @=> Core.ns["get"];
new MalIsContains @=> Core.ns["contains?"];
new MalKeys @=> Core.ns["keys"];
new MalVals @=> Core.ns["vals"];

new MalSequential @=> Core.ns["sequential?"];
new MalIsFn @=> Core.ns["fn?"];
new MalIsMacro @=> Core.ns["macro?"];

new MalReadline @=> Core.ns["readline"];
new MalMeta @=> Core.ns["meta"];
new MalWithMeta @=> Core.ns["with-meta"];

new MalTimeMs @=> Core.ns["time-ms"];
new MalConj @=> Core.ns["conj"];
new MalIsString @=> Core.ns["string?"];
new MalSeq @=> Core.ns["seq"];
