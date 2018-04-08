Reader {
	classvar tokenRe, numberRe;
	var tokens, index;

	*initClass {
		// the below fails compiling otherwise
		// https://github.com/supercollider/supercollider/issues/3352
		tokenRe = "[[:space:],]*(~@|[][{}()'`~^@]|\"" ++ // \"
		"(\\\\.|[^\\\"" ++ // \"
		"])*\"" ++ // \"
		// . matches everything, including the newline
		"|;[^\n]*|[^][[:space:]{}()'\"" ++ // \"
		"`,;]*)";
		// Emacs doesn't like char escapes inside strings :<
		numberRe = "^[+-]?[0-9]+$"; // "
	}

	*new {
		|tokens, index = 0|
		^super.newCopyArgs(tokens, index)
	}

	next {
		|token|
		token = tokens[index];
		index = index + 1;
		^token
	}

	peek {
		^tokens[index]
	}

	*readStr {
		|input|
		var reader = Reader(Reader.tokenizer(input));
		^Reader.readForm(reader)
	}

	*tokenizer {
		|input|
		var i = 0, match;
		var token, tokens = [];
		while { match = input.findRegexpAt(tokenRe, i); match[0].notEmpty } {
			token = match[0].sub("^[[:space:],]*", "");
			i = i + match[1];
			if (token[0] != $;) { tokens = tokens.add(token) }
		};
		if (i < input.size) { tokens = tokens.add(input[i..]) };
		^tokens
	}

	*readForm {
		|reader|
		var token = reader.peek;
		if (token.isNil) { MALEmptyInputError("empty input").throw };
		if (token == "'") { ^Reader.readSimpleMacro(reader, \quote) };
		if (token == "`") { ^Reader.readSimpleMacro(reader, \quasiquote) };
		if (token == "~") { ^Reader.readSimpleMacro(reader, \unquote) };
		if (token == "~@") { ^Reader.readSimpleMacro(reader, 'splice-unquote') };
		if (token == "@") { ^Reader.readSimpleMacro(reader, \deref) };
		if (token == "^") { ^Reader.readMetaMacro(reader) };
		if (token == "(") { ^Reader.readList(reader, MALList, ")") };
		if (token == ")") { ^MALError("unexpected ')'").throw };
		if (token == "[") { ^Reader.readList(reader, MALVector, "]") };
		if (token == "]") { ^MALError("unexpected ']'").throw };
		if (token == "{") { ^Reader.readList(reader, MALMap, "}") };
		if (token == "}") { ^MALError("unexpected '}'").throw };
		^Reader.readAtom(reader)
	}

	*readSimpleMacro {
		|reader, name|
		reader.next; // pop starter token
		^MALList(List.newUsing([MALSymbol(name), Reader.readForm(reader)]))
	}

	*readMetaMacro {
		|reader|
		var form, meta;
		reader.next;
		meta = Reader.readForm(reader);
		form = Reader.readForm(reader);
		^MALList(List.newUsing([MALSymbol('with-meta'), form, meta]))
	}

	*readList {
		|reader, class, ender|
		var form, storage = List.new;
		reader.next; // pop starter token
		while { reader.peek.notNil } {
			if (reader.peek == ender) {
				reader.next;
				if (class == MALMap) { storage = Dictionary.newFrom(storage) };
				^class.new(storage)
			};
			form = Reader.readForm(reader);
			storage.add(form)
		};
		MALError("expected '" ++ ender ++ "', got EOF").throw
	}

	*readAtom {
		|reader|
		var token = reader.next;
		if (token == "true") { ^MALObject.t };
		if (token == "false") { ^MALObject.f };
		if (token == "nil") { ^MALObject.n };
		if (token[0] == $") {
			if (token.size > 1 && token[token.size - 1] == $") {
				^MALString(token.interpret)
			} { MALError("expected '\"', got EOF").throw }
		};
		if (token[0] == $:) {
			^MALKeyword(token[1..].asSymbol)
		};
		if (numberRe.matchRegexp(token)) { ^MALInt(token.asInteger) };
		^MALSymbol(token.asSymbol)
	}
}
