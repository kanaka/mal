use io;
use fmt;

export type NotImplemented = !void;
export type unexpectedEOF = !void;
export type unbalanced = !void;
export type error = !(io::error | unexpectedEOF | unbalanced | NotImplemented);

export fn handle_error(e: error) void = {
	match(e){
	case unexpectedEOF =>
		fmt::errorln("Unexpected EOF!")!;
	case unbalanced =>
		fmt::errorln("Unbalanced Delimiters")!;
	case =>
		fmt::fatal("unknown error");
	};
};
