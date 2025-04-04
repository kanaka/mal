use io;
use fmt;
use os;

export type NotImplemented = !void;
export type unexpectedEOF = !void;
export type unbalanced = !void;

export type undefinedSymbol = !(void, symbol);
export type syntaxError = !(str, MalType);
export type typeerror = !(str, MalType);

export type error = !(io::error | unexpectedEOF | unbalanced | NotImplemented | undefinedSymbol | syntaxError | typeerror);

export fn handle_error(e: error) void = {
	match(e){
	case let e: typeerror =>
		fmt::error("Type Error: expected:", e.0, "got:")!;
		print_form(os::stderr, e.1);
		fmt::error("\n")!;
	case let e: syntaxError =>
		fmt::errorln("Syntax Error", e.0)!;
		print_form(os::stderr, e.1);
		fmt::error("\n")!;
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
