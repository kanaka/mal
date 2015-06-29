library mal.reader;

import "types.dart";

class Reader {
  final List<String> tokens;
  int _tokenIndex = 0;

  Reader(this.tokens);

  String next() {
    return tokens[_tokenIndex++];
  }

  String peek() {
    if (_tokenIndex >= tokens.length) {
      return null;
    } else {
      return tokens[_tokenIndex];
    }
  }
}

MalType read_str(String str) {
  List<String> tokens = tokenizer(str);

  if (tokens.length == 0) {
    // return null;
    // TODO(adam): Should return MalType null or empty string
    return new MalString("");
  }

  Reader reader = new Reader(tokens);
  return read_form(reader);
}

List<String> tokenizer(String str) {
  String regexStr = r"""[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)""";

  RegExp regex = new RegExp(regexStr);

  // TODO(adam): trying to parse "abc will always fail, the regex doesn't support it.
  List<String> tokens = regex
    .allMatches(str)
    .map((Match match) => match.group(1))
    .where((String t) => t != null && !(t == "") && t[0] != ';').toList();

  return tokens;
}

MalType read_form(Reader reader) {
  String token = reader.peek();
  MalType form;

  if (token == null) {
    return null;
  }

  switch(token) {
    case "'":
      reader.next();
      return new MalList.fromList([new MalSymbol("quote"), read_form(reader)]);

    case "`":
      reader.next();
      return new MalList.fromList([new MalSymbol("quasiquote"), read_form(reader)]);

    case "~":
      reader.next();
      return new MalList.fromList([new MalSymbol("unquote"), read_form(reader)]);

    case "^":
      reader.next();
      MalType meta = read_form(reader);
      return new MalList.fromList([new MalSymbol("with-meta"), read_form(reader), meta]);

    case "~@":
      reader.next();
      return new MalList.fromList([new MalSymbol("splice-unquote"), read_form(reader)]);

    case "@":
      reader.next();
      return new MalList.fromList([new MalSymbol("deref"), read_form(reader)]);

    case ")":
      throw new StateError("unexpected ')'");

    case "(":
      form = read_list(reader, new MalList(), start: "(", end: ")");
    break;

    case "[":
      form = read_list(reader, new MalVector(), start: "[", end: "]");
    break;

    case "]":
      throw new StateError("unexpected ']'");

    case "{":
      form = read_hash_map(reader);
    break;

    case "}":
      throw new StateError("unexpected '}'");

    default:
      form = read_atom(reader);
    break;
  }

  return form;
}

MalType read_list(Reader reader, MalList malList, {String start: "(", String end: ")"}) {
  String token = reader.next();

  if (token[0] != start) {
    throw new StateError("expected '$start'");
  }

  while ((token = reader.peek()) != null && token[0] != end) {
    malList.malTypes.add(read_form(reader));
  }

  if (token == null) {
    throw new StateError("expected '$end', got EOF");
  }

  reader.next();

  return malList;
}

MalType read_hash_map(Reader reader) {
  MalList malList = read_list(reader, new MalList(), start: '{', end: '}');
  return new MalHashMap.fromMalList(malList);
}

MalType read_atom(Reader reader) {
  String token = reader.next();

  if (token == null) {
    throw new StateError("unrecognized token '${token}'");
  }

  String numPattern = r"""(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)""";
  RegExp numRegex = new RegExp(numPattern);

  String strPattern = r"""^"(.*)"$""";
  RegExp strRegex = new RegExp(strPattern);

  String symPattern = r"""(^[^""]*$)""";
  RegExp symRegex = new RegExp(symPattern);

  if (numRegex.hasMatch(token)) {
    return new MalNumber(num.parse(token));
  } else if (strRegex.hasMatch(token)) {
    String str = token;
    if (str[0] == '"' && str[str.length-1] == '"') {
      str = str.substring(1,str.length-1);
    }

    return new MalString(str);
  } else if (symRegex.hasMatch(token)) {

    // TODO(adam): clean up the nested if/else logic.
    if (token.startsWith(":")) {
      return new MalKeyword(token);
    }

    switch(token.toLowerCase()) {
      case "true": return MAL_TRUE;
      case "false": return MAL_FALSE;
      case "nil": return MAL_NIL;
      default: return new MalSymbol(token);
    }
  } else {
    throw new StateError("unrecognized token '${token}'");
  }
}
