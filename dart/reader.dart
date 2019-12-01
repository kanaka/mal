import 'types.dart';

final malRegExp = RegExp(
    r"""[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)""");
final strRegExp = RegExp(r"""^"(?:\\.|[^\\"])*"$""");

class Reader {
  final List<String> tokens;
  int _position = 0;

  Reader(this.tokens);

  String next() {
    var token = peek();
    _position++;
    return token;
  }

  String peek() {
    if (_position >= tokens.length) return null;
    return tokens[_position];
  }
}

class ParseException implements Exception {
  final String message;

  ParseException(this.message);
}

class NoInputException implements Exception {}

MalType read_str(String code) {
  var tokens = tokenizer(code);
  if (tokens.isEmpty) {
    throw NoInputException();
  }
  var reader = Reader(tokens);
  return read_form(reader);
}

List<String> tokenizer(String code) {
  var matches = malRegExp.allMatches(code);
  return matches
      .map((m) => m.group(1))
      .where((token) => token.isNotEmpty && !token.startsWith(';'))
      .toList();
}

MalType read_form(Reader reader) {
  const macros = <String, String>{
    "'": 'quote',
    '`': 'quasiquote',
    '~': 'unquote',
    '~@': 'splice-unquote',
    '@': 'deref',
    '^': 'with-meta',
  };
  const sequenceStarters = <String, String>{'(': ')', '[': ']', '{': '}'};
  var token = reader.peek();
  if (sequenceStarters.containsKey(token)) {
    var elements = read_sequence(reader, token, sequenceStarters[token]);
    if (token == '(') {
      return MalList(elements);
    }
    if (token == '[') {
      return MalVector(elements);
    }

    if (token == '{') {
      return MalHashMap.fromSequence(elements);
    }

    throw StateError("Impossible!");
  } else if (macros.containsKey(token)) {
    var macro = MalSymbol(macros[token]);
    reader.next();
    var form = read_form(reader);
    if (token == '^') {
      var meta = read_form(reader);
      return MalList([macro, meta, form]);
    } else {
      return MalList([macro, form]);
    }
  } else {
    return read_atom(reader);
  }
}

List<MalType> read_sequence(Reader reader, String open, String close) {
  // Consume opening token
  var actualOpen = reader.next();
  assert(actualOpen == open);

  var elements = <MalType>[];
  for (var token = reader.peek();; token = reader.peek()) {
    if (token == null) {
      throw ParseException("expected '$close', got EOF");
    }
    if (token == close) break;
    elements.add(read_form(reader));
  }

  var actualClose = reader.next();
  assert(actualClose == close);

  return elements;
}

MalType read_atom(Reader reader) {
  var token = reader.next();

  var intAtom = int.tryParse(token);
  if (intAtom != null) {
    return MalInt(intAtom);
  }

  if (strRegExp.matchAsPrefix(token) != null) {
    var sanitizedToken = token
        // remove surrounding quotes
        .substring(1, token.length - 1)
        .replaceAllMapped(
            RegExp("\\\\(.)"), (Match m) => m[1] == 'n' ? '\n' : m[1]);
    return MalString(sanitizedToken);
  }

  if (token[0] == '"') {
    throw ParseException("expected '\"', got EOF");
  }

  if (token[0] == ':') {
    return MalKeyword(token.substring(1));
  }

  if (token == 'nil') {
    return MalNil();
  }

  if (token == 'true') {
    return MalBool(true);
  }

  if (token == 'false') {
    return MalBool(false);
  }

  return MalSymbol(token);
}
