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
		MALSymbol, { env.atFail(sexp.value) {
			MALError("'%' not found".format(sexp.value)).throw
		} },
		MALList, { MALList(evalList.(sexp, env, List)) },
		MALVector, { MALVector(evalList.(sexp, env, List)) },
		MALMap, { MALMap(evalList.(sexp, env, Dictionary)) },
		{ sexp })
};

var evalList = {
	|sexp, env, class|
	class.newFrom(sexp.value.collect { |item| eval.(item, env) })
};

var eval = {
	|sexp, env|
	var op, args;
	if (sexp.class == MALList) {
		if (sexp.value.notEmpty) {
			# op ...args = evalAst.(sexp, env).value;
			op.value(*args)
		} { sexp }
	} { evalAst.(sexp, env) }
};

var print = {
	|sexp|
	Printer.prStr(sexp, true)
};

var replEnv = Dictionary.newFrom([
	'+', { |a, b| MALInt(a.value + b.value) },
	'-', { |a, b| MALInt(a.value - b.value) },
	'*', { |a, b| MALInt(a.value * b.value) },
	'/', { |a, b| MALInt(a.value / b.value) }
]);

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

main.();
exit(0);
