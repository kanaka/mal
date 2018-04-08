MAL2 {
	var replEnv;

	*new {
		var replEnv = Dictionary.newFrom([
			'+', { |a, b| MALInt(a.value + b.value) },
			'-', { |a, b| MALInt(a.value - b.value) },
			'*', { |a, b| MALInt(a.value * b.value) },
			'/', { |a, b| MALInt(a.value / b.value) }
		]);
		^super.newCopyArgs(replEnv)
	}

	read {
		|input|
		^Reader.readStr(input)
	}

	evalAst {
		|sexp, env|
		^switch (sexp.class,
			MALSymbol, { env.atFail(sexp.value) {
				MALError("'%' not found".format(sexp.value)).throw
			} },
			MALList, { MALList(this.evalList(sexp, env, List)) },
			MALVector, { MALVector(this.evalList(sexp, env, List)) },
			MALMap, { MALMap(this.evalList(sexp, env, Dictionary)) },
			{ sexp })
	}

	evalList {
		|sexp, env, class|
		^class.newFrom(sexp.value.collect { |item| this.eval(item, env) })
	}

	eval {
		|sexp, env|
		var op, args;
		if (sexp.class != MALList) { ^this.evalAst(sexp, env) };
		if (sexp.value.isEmpty) { ^sexp };
		# op ...args = this.evalAst(sexp, env).value;
		^op.value(*args)
	}

	print {
		|sexp|
		^Printer.prStr(sexp, true)
	}

	rep {
		|input|
		^this.print(this.eval(this.read(input), replEnv));
	}

	repl {
		var line;
		while { line = "user> ".readline; line.notNil } {
			try { this.rep(line).postln } {
				|err|
				switch (err.class,
					MALEmptyInputError, {},
					MALError, { "error: %\n".postf(err.errorString) },
					{ err.reportError }
				)
			}
		}
	}
}
