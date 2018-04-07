MAL1 {
	read {
		|input|
		^Reader.readStr(input)
	}

	eval {
		|sexp|
		^sexp
	}

	print {
		|sexp|
		^Printer.prStr(sexp, true)
	}

	rep {
		|input|
		^this.print(this.eval(this.read(input)))
	}

	repl {
		var line;
		while { line = "user> ".readline; line.notNil } {
			try { this.rep(line).postln } {
				|err|
				switch (err.class,
					MALEmptyInputError, {},
					MALError, { "error: %\n".postf(err.what) },
					{ err.reportError }
				)
			}
		}
	}
}
