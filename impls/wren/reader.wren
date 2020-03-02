import "./types" for MalVal, MalSymbol, MalList, MalVector, MalMap

class Tokenizer {
  construct new(s) {
    _s = s
  }

  tokenize() {
    _pos = 0
    var tokens = []
    while (true) {
      var token = nextToken()
      if (token == null) break
      if (token.count > 0) tokens.add(token)
    }
    return tokens
  }

  static eolChars { "\r\n" }
  static whitespace { " ,\r\n\t" }
  static delimiters { "[]{}()'`^@" }
  static separators { Tokenizer.whitespace + "[]{}()'\"`,;" }

  nextToken() {
    if (isEOF()) return null
    var ch = curr
    if (Tokenizer.whitespace.contains(ch)) {
      advance()
      return ""
    }
    if (Tokenizer.delimiters.contains(ch)) {
      advance()
      return ch
    }
    if (ch == "~") {
      advance()
      if (!isEOF() && curr == "@") {
        advance()
        return "~@"
      } else {
        return "~"
      }
    }
    if (ch == ";") {
      advance()
      while (!isEOF() && !Tokenizer.eolChars.contains(curr)) advance()
      return ""
    }
    if (ch == "\"") {
      var s = ch
      advance()
      while (!isEOF() && curr != "\"") {
        if (curr == "\\") {
          s = s + curr
          advance()
          if (isEOF()) Fiber.abort("expected '\"', got EOF 111")
        }
        s = s + curr
        advance()
      }
      if (isEOF()) Fiber.abort("expected '\"', got EOF 222")
      s = s + curr
      advance()
      return s
    }
    var token = ch
    advance()
    while (!isEOF() && !Tokenizer.separators.contains(curr)) {
      token = token + curr
      advance()
    }
    return token
  }

  curr { _s[_pos] }
  isEOF() { _pos >= _s.count }
  advance() { _pos = _pos + 1 }
}

class Reader {
  construct new(tokens) {
    _tokens = tokens
    _pos = 0
  }

  next() {
    if (_pos >= _tokens.count) return null
    var token = _tokens[_pos]
    _pos = _pos + 1
    return token
  }

  peek() {
    if (_pos >= _tokens.count) return null
    return _tokens[_pos]
  }
}

class MalReader {
  static parse_str(token) {
    if (token.count <= 2) return ""
    return token[1..-2].replace("\\\\", "\u029e").replace("\\\"", "\"").replace("\\n", "\n").replace("\u029e", "\\")
  }

  static is_all_digits(s) {
    if (s.count == 0) return false
    return s.all { |c| c.bytes[0] >= 0x30 && c.bytes[0] <= 0x39 }
  }

  static is_number(token) {
    return token.startsWith("-") ? is_all_digits(token[1..-1]) : is_all_digits(token)
  }

  static read_atom(rdr) {
    var token = rdr.next()
    if (is_number(token)) return Num.fromString(token)
    if (token.startsWith("\"")) return parse_str(token)
    if (token.startsWith(":")) return MalVal.newKeyword(token[1..-1])
    if (token == "nil") return null
    if (token == "true") return true
    if (token == "false") return false
    return MalSymbol.new(token)
  }

  static read_seq(rdr, start, end) {
    var token = rdr.next()
    if (token != start) Fiber.abort("expected '%(start)'")
    var elements = []
    token = rdr.peek()
    while (token != end) {
      if (!token) Fiber.abort("expected '%(end)', got EOF")
      elements.add(read_form(rdr))
      token = rdr.peek()
    }
    rdr.next()
    return elements
  }

  static reader_macro(rdr, sym) {
    rdr.next()
    return MalList.new([MalSymbol.new(sym), read_form(rdr)])
  }

  static read_form(rdr) {
    var token = rdr.peek()
    if (token == "'") return reader_macro(rdr, "quote")
    if (token == "`") return reader_macro(rdr, "quasiquote")
    if (token == "~") return reader_macro(rdr, "unquote")
    if (token == "~@") return reader_macro(rdr, "splice-unquote")
    if (token == "^") {
      rdr.next()
      var meta = read_form(rdr)
      return MalList.new([MalSymbol.new("with-meta"), read_form(rdr), meta])
    }
    if (token == "@") return reader_macro(rdr, "deref")
    if (token == "(") return MalList.new(read_seq(rdr, "(", ")"))
    if (token == ")") Fiber.abort("unexpected ')'")
    if (token == "[") return MalVector.new(read_seq(rdr, "[", "]"))
    if (token == "]") Fiber.abort("unexpected ']'")
    if (token == "{") return MalMap.fromList(read_seq(rdr, "{", "}"))
    if (token == "}") Fiber.abort("unexpected '}'")
    return read_atom(rdr)
  }

  static read_str(s) {
    var tokens = Tokenizer.new(s).tokenize()
    if (tokens.count == 0) return null
    return read_form(Reader.new(tokens))
  }
}
