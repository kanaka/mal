MAL5 {
	var replEnv, specialForms;

	*new {
		var replEnv = MALEnv(nil), specialForms = ['def!', 'let*',
			'do', 'if', 'fn*'];
		Core.ns.pairsDo { |key, value| replEnv.set(key, value) };
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
		var a0, a1, a2, a3, op, args;
		if (sexp.class != MALList) { ^this.evalAst(sexp, env) };
		if (sexp.value.isEmpty) { ^sexp };
		# a0, a1, a2, a3 = sexp.value;
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
					^this.eval(a2, env_) // TCO
				},
				'do', {
					var forms = sexp.value.copyRange(1, sexp.value.size - 2);
					forms.do { |form| this.eval(form, env) };
					^this.eval(sexp.value[sexp.value.size - 1], env) // TCO
				},
				'if', {
					var condition = this.eval(a1, env);
					if ([MALFalse, MALNil].includes(condition.class)) {
						if (a3.notNil) {
							^this.eval(a3, env) // TCO
						} { ^MALObject.n }
					} {
						^this.eval(a2, env) // TCO
					}
				},
				'fn*', {
					var binds = a1.value.collect(_.value),
					fn = { |...args| this.eval(a2, MALEnv(env, binds, args)) };
					^Func(a2, binds, env, fn)
				},
				{ "unknown special form".error.throw })
		} {
			# op ...args = this.evalAst(sexp, env).value;
			if (op.class == Func) {
				^this.eval(op.ast, MALEnv(op.env, op.params, args)) // TCO
			} { ^op.value(*args) }
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
		this.rep("(def! not (fn* (a) (if a false true)))");
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
