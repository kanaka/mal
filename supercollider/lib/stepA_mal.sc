MALA {
	var replEnv, specialForms;

	*new {
		var replEnv = MALEnv(nil), specialForms = ['def!', 'let*',
			'do', 'if', 'fn*', 'quote', 'quasiquote',
			'defmacro!', 'macroexpand', 'try*'];
		^super.newCopyArgs(replEnv, specialForms).init
	}

	init {
		var argv = thisProcess.argv;
		Core.ns.pairsDo { |key, value| replEnv.set(key, value) };
		replEnv.set('eval', { |ast| this.eval(ast, replEnv) });
		replEnv.set('*ARGV*', MALList(List.newFrom(argv[1..].collect { |x| MALString(x) })));
		replEnv.set('*host-language*', MALString("supercollider"));
		this.rep("(def! not (fn* (a) (if a false true)))");
		this.rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
		this.rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
		this.rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))");
		this.rep("(def! *gensym-counter* (atom 0))");
		this.rep("(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))");
		this.rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))");
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

	isPair {
		|sexp|
		^[MALList, MALVector].includes(sexp.class) and: { sexp.value.notEmpty }
	}

	quasiquote {
		|ast|
		var a0, a1, a1_;
		if (this.isPair(ast).not) { ^MALList(List.newFrom([MALSymbol(\quote), ast])) };
		# a0, a1 = ast.value;
		a1_ = ast.value[1..];
		if (a0.class == MALSymbol and: { a0.value == \unquote }) { ^a1 };
		if (this.isPair(a0) and: { a0.value[0].class == MALSymbol &&
			a0.value[0].value == 'splice-unquote' }) {
				^MALList(List.newFrom([MALSymbol(\concat), a0.value[1],
					this.quasiquote(MALList(List.newFrom(a1_)))]))
			};
		^MALList(List.newFrom([MALSymbol(\cons), this.quasiquote(a0),
			this.quasiquote(MALList(List.newFrom(a1_)))]))
	}

	isMacroCall {
		|ast, env|
		var a0, def;
		if (ast.class != MALList) { ^false };
		if (ast.value.isEmpty) { ^false };
		a0 = ast.value[0];
		if (a0.class != MALSymbol) { ^false };
		def = env.find(a0.value);
		^def.class == Func and: { def.isMacro }
	}

	macroexpand {
		|ast, env|
		while { this.isMacroCall(ast, env) } {
			var symbol = ast.value[0].value, macro = env.get(symbol);
			ast = macro.fn.value(*(ast.value[1..]))
		};
		^ast
	}

	eval {
		|sexp, env|
		var a0, a1, a2, a3, op, args;
		if (sexp.class != MALList) { ^this.evalAst(sexp, env) };
		sexp = this.macroexpand(sexp, env);
		if (sexp.class != MALList) { ^this.evalAst(sexp, env) };
		if (sexp.value.isEmpty) { ^sexp };
		# a0, a1, a2, a3 = sexp.value;
		if (a0.class == MALSymbol && specialForms.includes(a0.value)) {
			switch(a0.value,
				'def!', {
					^env.set(a1.value, this.eval(a2, env))
				},
				'defmacro!', {
					var value = this.eval(a2, env);
					value.isMacro = true;
					^env.set(a1.value, value)
				},
				'macroexpand', {
					^this.macroexpand(a1, env)
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
				'quote', {
					^a1
				},
				'quasiquote', {
					^this.eval(this.quasiquote(a1), env) // TCO
				},
				'try*', {
					var result;
					try { result = this.eval(a1, env) } {
						|err|
						var what = err.what, sym, form, env_;
						if (what.class == String) { what = MALString(what) };
						sym = a2.value[1].value;
						form = a2.value[2];
						env_ = MALEnv(env, [sym], [what]);
						result = this.eval(form, env_)
					};
					^result
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
		// the commented out line fails evaluating (try* (x) (catch* e e))
		// ^this.print(this.eval(this.read(input), replEnv))
		var sexp = this.read(input), result = this.eval(sexp, replEnv);
		^this.print(result)
	}

	repl {
		var line;
		this.rep("(println (str \"Mal [\" \*host-language\* \"]\"))");
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
