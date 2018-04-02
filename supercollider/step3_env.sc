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

var specialForms = ['def!', 'let*'];

var eval = {
	|sexp, env|
	var a0, a1, a2, env_, op, args;
	if (sexp.class == MALList) {
		# a0, a1, a2 = sexp.value;
		if (sexp.value.notEmpty) {
			if (a0.class == MALSymbol && specialForms.includes(a0.value)) {
				switch(a0.value,
					'def!', {
						env.set(a1.value, eval.(a2, env))
					},
					'let*', {
						env_ = MALEnv(env);
						a1.value.pairsDo {
							|key, value|
							env_.set(key.value, eval.(value, env_))
						};
						eval.(a2, env_)
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

replEnv.set('+', { |a, b| MALInt(a.value + b.value) });
replEnv.set('-', { |a, b| MALInt(a.value - b.value) });
replEnv.set('*', { |a, b| MALInt(a.value * b.value) });
replEnv.set('/', { |a, b| MALInt(a.value / b.value) });

"REAL OUTPUT HERE".postln;
main.();
exit(0);
