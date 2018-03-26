var stdin = File.open("/dev/stdin", "r");

var readline = {
	|prompt|
	prompt.post;
	stdin.getLine;
};

var read = {
	|input|
	input;
};

var eval = {
	|sexp|
	sexp;
};

var print = {
	|sexp|
	sexp;
};

var rep = {
	|input|
	print.(eval.(read.(input)));
};

var main = {
	var line;
	while { line = readline.("user> "); line.notNil } {
		rep.(line).postln
	};
};

main.();
exit(0);
