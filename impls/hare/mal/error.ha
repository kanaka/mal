use bufio;
use io;
use fmt;
use os;
use fs;

export type malerror = !(str, MalType);

export type not_implemented = !void;
export type unexpected_eof = !void;
export type unbalanced = !void;

export type undefined_key = !(str, (symbol | string));
export type undefined_symbol = !(str, symbol);
export type syntax_error = !(str, (MalType | []MalType));
export type type_error = !(str, (MalType | []MalType));

export type error = !(malerror | fs::error | io::error | unexpected_eof |
	unbalanced | not_implemented | undefined_symbol | undefined_key |
	syntax_error | type_error);

export fn format_error(strbuf: io::handle, e: error) void = {

	match(e){
	case let e: type_error =>
		fmt::fprint(strbuf, "Type Error: expected", e.0, "got:")!;
		print_form(strbuf, e.1, false);
		fmt::fprint(strbuf, "\n")!;
	case let e: syntax_error =>
		fmt::fprintln(strbuf, "Syntax Error:", e.0)!;
		print_form(strbuf, e.1, false);
		fmt::fprint(strbuf, "\n")!;
	case let e: undefined_symbol =>
		fmt::fprintf(strbuf, "'{}' not found", e.1)!;
		fmt::print("\n")!;
	case unexpected_eof =>
		fmt::fprintln(strbuf, "Unexpected EOF!")!;
	case let e: malerror =>
		print_form(strbuf, e.1, false);
		fmt::print("\n")!;
	case unbalanced =>
		fmt::fprintln(strbuf, "Unbalanced Delimiters")!;
	case not_implemented =>
		fmt::fprintln(strbuf, "not implemented")!;
	case let e: io::error =>
		fmt::fprintln(strbuf, io::strerror(e))!;
	case let e: fs::error =>
		fmt::fprintln(strbuf, fs::strerror(e))!;
	case =>
		fmt::fatal("unknown error");
	};
};
