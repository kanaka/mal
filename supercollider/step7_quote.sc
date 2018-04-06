var stdin = File.open("/dev/stdin", "r");

var readline = {
	|prompt|
	prompt.post;
	stdin.getLine;
};

var read = {
	|input|
	Reader.readStr(input)
};

var evalAst = {
	|sexp, env|
	switch (sexp.class,
		MALSymbol, { env.get(sexp.value) },
		MALList, { MALList(evalList.(sexp, env, List)) },
		MALVector, { MALVector(evalList.(sexp, env, List)) },
		MALMap, { MALMap(evalList.(sexp, env, Dictionary)) },
		{ sexp })
};

var evalList = {
	|sexp, env, class|
	class.newFrom(sexp.value.collect { |item| eval.(item, env) })
};

var specialForms = ['def!', 'let*', 'do', 'if', 'fn*', 'quote', 'quasiquote'];

var isPair = {
	|sexp|
	[MALList, MALVector].includes(sexp.class) and: { sexp.value.notEmpty }
};

var quasiquote = {
	|ast|
	var a0, a1, a1_;
	if (isPair.(ast).not) {
		MALList(List.newFrom([MALSymbol(\quote), ast]))
	} {
		# a0, a1 = ast.value;
		a1_ = ast.value[1..];
		if (a0.class == MALSymbol and: { a0.value == \unquote }) { a1 } {
			if (isPair.(a0) and: { a0.value[0].class == MALSymbol &&
					a0.value[0].value == 'splice-unquote' }) {
				MALList(List.newFrom([MALSymbol(\concat), a0.value[1],
					quasiquote.(MALList(List.newFrom(a1_)))]))
			} {
				MALList(List.newFrom([MALSymbol(\cons), quasiquote.(a0),
					quasiquote.(MALList(List.newFrom(a1_)))]))
			}
		}
	};
};

var eval = {
	|sexp, env|
	var a0, a1, a2, a2_, a3, op, args;
	if (sexp.class == MALList) {
		# a0, a1, a2, a3 = sexp.value;
		if (sexp.value.notEmpty) {
			if (a0.class == MALSymbol && specialForms.includes(a0.value)) {
				switch(a0.value,
					'def!', {
						env.set(a1.value, eval.(a2, env))
					},
					'let*', {
						var env_ = MALEnv(env);
						a1.value.pairsDo {
							|key, value|
							env_.set(key.value, eval.(value, env_))
						};
						eval.(a2, env_) // TCO
					},
					'do', {
						var forms = sexp.value.copyRange(1, sexp.value.size - 2);
						forms.do { |form| eval.(form, env) };
						eval.(sexp.value[sexp.value.size - 1], env) // TCO
					},
					'if', {
						var condition = eval.(a1, env);
						if ([MALFalse, MALNil].includes(condition.class)) {
							if (a3.notNil) {
								eval.(a3, env) // TCO
							} { sexp = MALObject.n }
						} {
							eval.(a2, env) // TCO
						}
					},
					'fn*', {
						var binds = a1.value.collect(_.value),
						fn = { |...args| eval.(a2, MALEnv(env, binds, args)) };
						Func(a2, binds, env, fn)
					},
					'quote', {
						a1
					},
					'quasiquote', {
						eval.(quasiquote.(a1), env) // TCO
					},
					{ "unknown special form".error.throw })
			} {
				# op ...args = evalAst.(sexp, env).value;
				if (op.class == Func) {
					eval.(op.ast, MALEnv(op.env, op.params, args)) // TCO
				} { op.value(*args) }
			}
		} { sexp }
	} { evalAst.(sexp, env) }
};

var print = {
	|sexp|
	Printer.prStr(sexp, true)
};

var replEnv = MALEnv(nil);

var rep = {
	|input|
	print.(eval.(read.(input), replEnv));
};

var main = {
	var line;
	while { line = readline.("user> "); line.notNil } {
		try { rep.(line).postln } {
			|err|
			switch (err.class,
				MALEmptyInputError, {},
				MALError, { "error: %\n".postf(err.what) },
				{ err.reportError }
			)
		}
	};
};

var argv = "SC_ARGS".getenv.split($\v).reject { |x| x.isEmpty };

Core.ns.pairsDo { |key, value| replEnv.set(key, value) };
replEnv.set('eval', { |ast| eval.(ast, replEnv) });
replEnv.set('*ARGV*', MALList(List.newFrom(argv[1..].collect { |x| MALString(x) })));

rep.("(def! not (fn* (a) (if a false true)))");
rep.("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

"REAL OUTPUT HERE".postln;
if (argv.notEmpty) {
	rep.("(load-file \"%\")".format(argv[0]));
} { main.() };

exit(0);
