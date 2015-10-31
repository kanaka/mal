" reader module

let Reader = {}

function NewReader(tokens)
  let r = copy(g:Reader)
  let r.tokens = a:tokens
  let r.pos = 0
  return r
endfunction

function Reader.peek() dict
  return self.tokens[self.pos]
endfunction

function Reader.nexttoken() dict
  let self.pos = self.pos + 1
  return self.tokens[self.pos - 1]
endfunction

function Tokenize(str)
  let tokenize_pat = "[[:blank:]\\n,]*" .
                   \ "\\(" .
                   \   "\\~@\\|" .
                   \   "[\\[\\]{}()'`~^@]\\|" .
                   \   "\"\\%(\\\\.\\|[^\\\\\"]\\)*\"\\|" .
                   \   ";[^\\n]*\\|" .
                   \   "[^[:blank:]\\n\\[\\]{}('\"`,;)]*" .
                   \ "\\)"
  let tokens = []
  let pos = 0
  while 1
    let mat = matchlist(a:str, tokenize_pat, pos)
    if len(mat) == 0 || mat[0] == ""
      break
    endif
    if mat[1] != "" && mat[1][0] != ";"
      call add(tokens, mat[1])
    endif
    let pos = matchend(a:str, tokenize_pat, pos)
  endwhile
  return tokens
endfunction

function ParseString(token)
  let str = a:token[1:-2]
  let str = substitute(str, '\\"', '"', "g")
  let str = substitute(str, '\\n', "\n", "g")
  let str = substitute(str, '\\\\', "\\", "g")
  return str
endfunction

function ReadAtom(rdr)
  let token = a:rdr.nexttoken()
  if token =~ "^-\\?[0-9]\\+$"
    return IntegerNew(str2nr(token))
  elseif token =~ "^-\\?[0-9][0-9.]*$"
    return FloatNew(str2float(token))
  elseif token =~ "^\".*\"$"
    return StringNew(ParseString(token))
  elseif token =~ "^:"
    return KeywordNew(token[1:-1])
  elseif token == "nil"
    return g:MalNil
  elseif token == "true"
    return TrueNew()
  elseif token == "false"
    return FalseNew()
  else
    return SymbolNew(token)
  endif
endfunction

function ReadTokensList(rdr, start, last)
  let elements = []
  let token = a:rdr.nexttoken()
  if token != a:start
    throw "expected '" . a:start . "'"
  endif
  let token = a:rdr.peek()
  while token != a:last
    if token == ""
      throw "expected '" . a:last . "', got EOF"
    endif
    call add(elements, ReadForm(a:rdr))
    let token = a:rdr.peek()
  endwhile
  call a:rdr.nexttoken()
  return elements
endfunction

function ReadList(rdr)
  let elements = ReadTokensList(a:rdr, "(", ")")
  return ListNew(elements)
endfunction

function ReadVector(rdr)
  let elements = ReadTokensList(a:rdr, "[", "]")
  return VectorNew(elements)
endfunction

function ReadHash(rdr)
  let elements = ReadTokensList(a:rdr, "{", "}")
  return HashBuild(elements)
endfunction

function ReadForm(rdr)
  let token = a:rdr.peek()
  if token == ";"
    return ""
  elseif token == "'"
    call a:rdr.nexttoken()
    return ListNew([SymbolNew("quote"), ReadForm(a:rdr)])
  elseif token == "`"
    call a:rdr.nexttoken()
    return ListNew([SymbolNew("quasiquote"), ReadForm(a:rdr)])
  elseif token == "~"
    call a:rdr.nexttoken()
    return ListNew([SymbolNew("unquote"), ReadForm(a:rdr)])
  elseif token == "~@"
    call a:rdr.nexttoken()
    return ListNew([SymbolNew("splice-unquote"), ReadForm(a:rdr)])
  elseif token == "^"
    call a:rdr.nexttoken()
    let meta = ReadForm(a:rdr)
    return ListNew([SymbolNew("with-meta"), ReadForm(a:rdr), meta])
  elseif token == "@"
    call a:rdr.nexttoken()
    return ListNew([SymbolNew("deref"), ReadForm(a:rdr)])
  elseif token == "("
    return ReadList(a:rdr)")
  elseif token == ")"
    throw "unexpected ')'"
  elseif token == "["
    return ReadVector(a:rdr)
  elseif token == "]"
    throw "unexpected ']'"
  elseif token == "{"
    return ReadHash(a:rdr)
  elseif token == "}"
    throw "unexpected '}'"
  else
    return ReadAtom(a:rdr)
  endif
endfunction

function ReadStr(str)
  let tokens = Tokenize(a:str)
  if empty(tokens)
    return ""
  endif
  return ReadForm(NewReader(tokens))
endfunction
