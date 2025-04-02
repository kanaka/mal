use io;
use fmt;

export type NotImplemented = !void;
export type unexpectedEOF = !void;
export type unbalanced = !void;

export type undefinedSymbol = !(void, symbol);
export type syntaxError = !(str, MalType);

export type error = !(io::error | unexpectedEOF | unbalanced | NotImplemented | undefinedSymbol | syntaxError);

export fn handle_error(e: error) void = {
	match(e){
	case let e: syntaxError =>
		fmt::errorln("Syntax Error", e.0)!;
		// print_form(e.1);
	case let e: undefinedSymbol =>
		fmt::errorln("Symbol", e.1, "not found")!;
	case unexpectedEOF =>
		fmt::errorln("Unexpected EOF!")!;
	case unbalanced =>
		fmt::errorln("Unbalanced Delimiters")!;
	case =>
		fmt::fatal("unknown error");
	};
};
