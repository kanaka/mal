MAL3 {
	var replEnv, specialForms;

	*new {
		var replEnv = MALEnv(nil), specialForms = ['def!', 'let*'];
		replEnv.set('+', { |a, b| MALInt(a.value + b.value) });
		replEnv.set('-', { |a, b| MALInt(a.value - b.value) });
		replEnv.set('*', { |a, b| MALInt(a.value * b.value) });
		replEnv.set('/', { |a, b| MALInt(a.value / b.value) });
		^super.newCopyArgs(replEnv, specialForms)
	}

	read {
		|input|
		^Reader.readStr(input)
	}

	evalAst {
		|sexp, env|
		^switch (sexp.class,
			MALSymbol, { env.get(sexp.value) },
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
		var a0, a1, a2, op, args;
		if (sexp.class != MALList) { ^this.evalAst(sexp, env) };
		if (sexp.value.isEmpty) { ^sexp };
		# a0, a1, a2 = sexp.value;
		if (a0.class == MALSymbol && specialForms.includes(a0.value)) {
			switch(a0.value,
				'def!', {
					^env.set(a1.value, this.eval(a2, env))
				},
				'let*', {
					var env_ = MALEnv(env);
					a1.value.pairsDo {
						|key, value|
						env_.set(key.value, this.eval(value, env_))
					};
					^this.eval(a2, env_)
				},
				{ "unknown special form".error.throw })
		} {
			# op ...args = this.evalAst(sexp, env).value;
			^op.value(*args)
		}
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
					MALError, { "error: %\n".postf(err.what) },
					{ err.reportError }
				)
			}
		}
	}
}
