MAL0 {
	read {
		|input|
		^input
	}

	eval {
		|sexp|
		^sexp
	}

	print {
		|sexp|
		^sexp
	}

	rep {
		|input|
		^this.print(this.eval(this.read(input)))
	}

	repl {
		var line;
		while { line = "user> ".readline; line.notNil } {
			this.rep(line).postln
		}
	}
}
