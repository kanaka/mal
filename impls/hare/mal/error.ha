use bufio;
use io;
use fmt;
use os;
use fs;

export type malerror = !(str, MalType);

export type NotImplemented = !void;
export type unexpectedEOF = !void;
export type unbalanced = !void;

export type undefinedKey = !(void, (symbol | string));
export type undefinedSymbol = !(void, symbol);
export type syntaxError = !(str, (MalType | []MalType));
export type typeerror = !(str, (MalType | []MalType));

export type error = !(malerror | fs::error | io::error | unexpectedEOF |
		      unbalanced | NotImplemented | undefinedSymbol |
		      undefinedKey | syntaxError | typeerror);

export fn handle_error(strbuf: io::handle, e: error) void = {

	match(e){
	case let e: typeerror =>
		fmt::fprint(strbuf, "Type Error: expected:", e.0, "got:")!;
		print_form(strbuf, e.1, false);
		fmt::fprint(strbuf, "\n")!;
	case let e: syntaxError =>
		fmt::fprintln(strbuf, "Syntax Error", e.0)!;
		print_form(strbuf, e.1, false);
		fmt::fprint(strbuf, "\n")!;
	case let e: undefinedSymbol =>
		fmt::fprintf(strbuf, "'{}' not found", e.1)!;
		fmt::print("\n")!;
	case unexpectedEOF =>
		fmt::fprintln(strbuf, "Unexpected EOF!")!;
	case let e: malerror =>
		print_form(strbuf, e.1, false);
		fmt::print("\n")!;
	case unbalanced =>
		fmt::fprintln(strbuf, "Unbalanced Delimiters")!;
	case NotImplemented =>
		fmt::fprintln(strbuf, "not implemented")!;
	case let e: io::error =>
		fmt::fprintln(strbuf, io::strerror(e))!;
	case let e: fs::error =>
		fmt::fprintln(strbuf, fs::strerror(e))!;
	case =>
		fmt::fatal("unknown error");
	};
};
