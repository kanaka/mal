internal class TokenReader
{
  const Str[] tokens
  private Int position := 0

  new make(Str[] new_tokens) { tokens = new_tokens }

  Str? peek()
  {
    if (position >= tokens.size) return null
    return tokens[position]
  }

  Str next() { return tokens[position++] }
}

class Reader
{
  private static Str[] tokenize(Str s)
  {
    r := Regex <|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|>
    m := r.matcher(s)
    tokens := Str[,]
    while (m.find())
    {
      token := m.group(1)
      if (token.isEmpty || token[0] == ';') continue
      tokens.add(m.group(1))
    }
    return tokens
  }

  private static Str unescape_str(Str s)
  {
    return s.replace("\\\\", "\u029e").replace("\\\"", "\"").replace("\\n", "\n").replace("\u029e", "\\")
  }

  private static MalVal read_atom(TokenReader reader)
  {
    token := reader.next
    intRegex := Regex <|^-?\d+$|>
    strRegex := Regex <|^"(?:\\.|[^\\"])*"|>
    strBadRegex := Regex <|^".*|>
    if (token == "nil") return MalNil.INSTANCE
    if (token == "true") return MalTrue.INSTANCE
    if (token == "false") return MalFalse.INSTANCE
    if (intRegex.matches(token)) return MalInteger(token.toInt)
    if (strRegex.matches(token)) return MalString.make(unescape_str(token[1..-2]))
    if (strBadRegex.matches(token)) throw Err("expected '\"', got EOF")
    if (token[0] == '"') return MalString.make(unescape_str(token[1..-2]))
    if (token[0] == ':') return MalString.makeKeyword(token[1..-1])
    return MalSymbol(token)
  }

  private static MalVal[] read_seq(TokenReader reader, Str open, Str close)
  {
    reader.next
    values := MalVal[,]
    token := reader.peek
    while (token != close)
    {
      if (token == null) throw Err("expected '$close', got EOF")
      values.add(read_form(reader))
      token = reader.peek
    }
    if (token != close) throw Err("Missing '$close'")
    reader.next
    return values
  }

  private static MalVal read_form(TokenReader reader)
  {
    switch (reader.peek)
    {
      case "\'":
        reader.next
        return MalList([MalSymbol("quote"), read_form(reader)])
      case "`":
        reader.next
        return MalList([MalSymbol("quasiquote"), read_form(reader)])
      case "~":
        reader.next
        return MalList([MalSymbol("unquote"), read_form(reader)])
      case "~@":
        reader.next
        return MalList([MalSymbol("splice-unquote"), read_form(reader)])
      case "^":
        reader.next
        meta := read_form(reader)
        return MalList([MalSymbol("with-meta"), read_form(reader), meta])
      case "@":
        reader.next
        return MalList([MalSymbol("deref"), read_form(reader)])
      case "(": return MalList(read_seq(reader, "(", ")"))
      case ")": throw Err("unexpected ')'")
      case "[": return MalVector(read_seq(reader, "[", "]"))
      case "]": throw Err("unexpected ']'")
      case "{": return MalHashMap.fromList(read_seq(reader, "{", "}"))
      case "}": throw Err("unexpected '}'")
      default:  return read_atom(reader)
    }
  }

  static MalVal read_str(Str s)
  {
    return read_form(TokenReader(tokenize(s)));
  }
}
