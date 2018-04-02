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

var eval = {
	|sexp|
	sexp
};

var print = {
	|sexp|
	Printer.prStr(sexp, true)
};

var rep = {
	|input|
	print.(eval.(read.(input)));
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

"REAL OUTPUT HERE".postln;
main.();
exit(0);
