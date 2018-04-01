Printer {
	*prStr {
		|sexp, printReadably|
		^switch (sexp.class,
			MALTrue, { "true" },
			MALFalse, { "false" },
			MALNil, { "nil" },
			MALInt, { sexp.value.asString },
			MALSymbol, { sexp.value.asString },
			MALString, { if (printReadably.notNil) {
				sexp.value.asCompileString.replace("\n", "\\n")
			} { sexp.value }
			},
			MALKeyword, { ":" ++ sexp.value },
			MALList, { Printer.prList(sexp.value, printReadably, "(", ")") },
			MALVector, { Printer.prList(sexp.value, printReadably, "[", "]") },
			MALMap, { Printer.prList(sexp.value.asPairs, printReadably, "{", "}") },
			Function, { "#<fn>" },
			{ "unknown type".error.throw })
	}

	*prList {
		|sexps, printReadably, starter, ender|
		var representations = sexps.collect {
			|sexp| Printer.prStr(sexp, printReadably)
		};
		^starter ++ representations.join(" ") ++ ender
	}
}
