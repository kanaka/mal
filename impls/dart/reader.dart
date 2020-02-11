import 'types.dart';

final malRegExp = new RegExp(
    r"""[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)""");
final strRegExp = new RegExp(
    r"""^"(?:\\.|[^\\"])*"$""");

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
    throw new NoInputException();
  }
  var reader = new Reader(tokens);
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
  const macros = const <String, String>{
    "'": 'quote',
    '`': 'quasiquote',
    '~': 'unquote',
    '~@': 'splice-unquote',
    '@': 'deref',
    '^': 'with-meta',
  };
  const sequenceStarters = const <String, String>{'(': ')', '[': ']', '{': '}'};
  var token = reader.peek();
  if (sequenceStarters.containsKey(token)) {
    var elements = read_sequence(reader, token, sequenceStarters[token]);
    if (token == '(') {
      return new MalList(elements);
    }
    if (token == '[') {
      return new MalVector(elements);
    }

    if (token == '{') {
      return new MalHashMap.fromSequence(elements);
    }

    throw new StateError("Impossible!");
  } else if (macros.containsKey(token)) {
    var macro = new MalSymbol(macros[token]);
    reader.next();
    var form = read_form(reader);
    if (token == '^') {
      var meta = read_form(reader);
      return new MalList([macro, meta, form]);
    } else {
      return new MalList([macro, form]);
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
      throw new ParseException("expected '$close', got EOF");
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

  var intAtom = int.parse(token, onError: (_) => null);
  if (intAtom != null) {
    return new MalInt(intAtom);
  }

  if (strRegExp.matchAsPrefix(token) != null) {
    var sanitizedToken = token
        // remove surrounding quotes
        .substring(1, token.length - 1)
        .replaceAllMapped(new RegExp("\\\\(.)"),
                          (Match m) => m[1] == 'n' ? '\n' : m[1]);
    return new MalString(sanitizedToken);
  }

  if (token[0] == '"') {
    throw new ParseException("expected '\"', got EOF");
  }

  if (token[0] == ':') {
    return new MalKeyword(token.substring(1));
  }

  if (token == 'nil') {
    return new MalNil();
  }

  if (token == 'true') {
    return new MalBool(true);
  }

  if (token == 'false') {
    return new MalBool(false);
  }

  return new MalSymbol(token);
}
