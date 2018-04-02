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

var specialForms = ['def!', 'let*', 'do', 'if', 'fn*'];

var eval = {
	|sexp, env|
	var a0, a1, a1_, a2, a3, op, args;
	if (sexp.class == MALList) {
		# a0, a1, a2, a3 = sexp.value;
		a1_ = sexp.value[1..];
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
						eval.(a2, env_)
					},
					'do', {
						var results = evalAst.(MALList(a1_), env).value;
						results[results.size - 1]
					},
					'if', {
						var condition = eval.(a1, env);
						if ([MALFalse, MALNil].includes(condition.class)) {
							if (a3.notNil) {
								eval.(a3, env)
							} { MALObject.n }
						} { eval.(a2, env) }
					},
					'fn*', {
						var binds = a1.value.collect { |item| item.value };
						{ |...args| eval.(a2, MALEnv(env, binds, args)) }
					},
					{ "unknown special form".error.throw })
			} {
				# op ...args = evalAst.(sexp, env).value;
				op.value(*args)
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

Core.ns.pairsDo { |key, value| replEnv.set(key, value) };
rep.("(def! not (fn* (a) (if a false true)))");

main.();
exit(0);
