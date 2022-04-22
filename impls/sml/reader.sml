exception Nothing
exception SyntaxError of string
exception ReaderError of string

structure Ss = Substring

datatype token =
    SPACE
    | COMMENT of string
    | BRACKET_LEFT | BRACKET_RIGHT
    | BRACE_LEFT   | BRACE_RIGHT
    | PAREN_LEFT   | PAREN_RIGHT
    | QUOTE | BACK_TICK | TILDE | TILDE_AT
    | CARET
    | AT
    | LIT_ATOM of string
    | LIT_STR of string

fun tokenString SPACE         = "SPACE"
  | tokenString (COMMENT s)   = "COMMENT (" ^ s ^ ")"
  | tokenString BRACKET_LEFT  = "BRACKET_LEFT"
  | tokenString BRACKET_RIGHT = "BRACKET_RIGHT"
  | tokenString BRACE_LEFT    = "BRACE_LEFT"
  | tokenString BRACE_RIGHT   = "BRACE_RIGHT"
  | tokenString PAREN_LEFT    = "PAREN_LEFT"
  | tokenString PAREN_RIGHT   = "PAREN_RIGHT"
  | tokenString QUOTE         = "QUOTE"
  | tokenString BACK_TICK     = "BACK_TICK"
  | tokenString TILDE         = "TILDE"
  | tokenString TILDE_AT      = "TILDE_AT"
  | tokenString CARET         = "CARET"
  | tokenString AT            = "AT"
  | tokenString (LIT_ATOM s)  = "LIT_ATOM (" ^ s ^ ")"
  | tokenString (LIT_STR s)   = "LIT_STR \"" ^ s ^ "\""

datatype reader = READER of token list

fun next (READER (x::xs)) = SOME (x, READER xs)
  | next r                = NONE

fun peek (READER (x::_)) = SOME x
  | peek r               = NONE

fun rest (READER (_::xs)) = READER xs
  | rest r = raise ReaderError "out of tokens"

fun findSpecial #"[" = SOME BRACKET_LEFT
  | findSpecial #"]" = SOME BRACKET_RIGHT
  | findSpecial #"(" = SOME PAREN_LEFT
  | findSpecial #")" = SOME PAREN_RIGHT
  | findSpecial #"{" = SOME BRACE_LEFT
  | findSpecial #"}" = SOME BRACE_RIGHT
  | findSpecial #"'" = SOME QUOTE
  | findSpecial #"`" = SOME BACK_TICK
  | findSpecial #"~" = SOME TILDE
  | findSpecial #"^" = SOME CARET
  | findSpecial #"@" = SOME AT
  | findSpecial _ = NONE

fun scanSpace ss =
    let fun isSpace c = Char.isSpace c orelse c = #","
        val (tok, rest) = Ss.splitl isSpace ss in
        if Ss.isEmpty tok then NONE else SOME (SPACE, rest)
    end

fun scanComment ss = case Ss.getc ss of
    SOME (#";", rest) =>
        let val (comment, rest) = Ss.splitl (fn (c) => c <> #"\n") rest in
            SOME (COMMENT (Ss.string comment), rest)
        end
    | _ => NONE

fun scanSpecial ss =
    if Ss.isPrefix "~@" ss
    then SOME (TILDE_AT, Ss.slice (ss, 2, NONE))
    else let fun findToken (c, rest) = findSpecial c |> Option.map (fn t => (t, rest)) in
        Option.composePartial (findToken, Ss.getc) ss
    end

fun scanString ss =
    Ss.getc ss |> Option.mapPartial (fn (#"\"", rest) => spanString rest rest | _ => NONE)

and spanString from to = case Ss.getc to of
    SOME (#"\\", rest)   => Ss.getc rest |> Option.mapPartial (fn (_, more) => spanString from more)
    | SOME (#"\"", rest) => SOME (LIT_STR (spanString' from to), rest)
    | SOME (_, rest)     => spanString from rest
    | NONE => raise SyntaxError "end of input reached when parsing string literal"
and spanString' from stop =
    Ss.span (from, Ss.slice (stop, 0, SOME 0)) |> Ss.string

fun scanAtom ss =
    let fun isAtomChar c = Char.isGraph c andalso (findSpecial c = NONE)
        val (tok, rest) = Ss.splitl isAtomChar ss in
        if Ss.isEmpty tok then NONE else SOME (LIT_ATOM (Ss.string tok), rest)
    end

fun scanToken ss =
    let val scanners = [scanSpace, scanComment, scanSpecial, scanString, scanAtom]
        val findScanner = List.find (fn f => isSome (f ss))
        fun applyScanner s = s ss
    in
        Option.composePartial (applyScanner, findScanner) scanners
    end

fun tokenize s = tokenize' [] (Ss.full s)
and tokenize' acc ss = case scanToken ss of
    SOME (token, rest) => tokenize' (token::acc) rest
    | NONE             => rev acc

fun readAtom r = case next r of
    SOME (LIT_ATOM "nil", r')     => (NIL, r')
    | SOME (LIT_ATOM "true", r')  => (BOOL true, r')
    | SOME (LIT_ATOM "false", r') => (BOOL false, r')
    | SOME (LIT_ATOM s, r')       => (LargeInt.fromString s |> Option.map INT
                                     |> optIfNone (fn () => Option.filter (String.isPrefix ":") s |> Option.map (KEYWORD o (triml 1)))
                                     |> valIfNone (fn () => SYMBOL s), r')
    | SOME (LIT_STR s, r')        => (malUnescape s |> STRING, r')
    | SOME (CARET, r')            => readWithMeta r'
    | SOME (token, _) => raise SyntaxError ("unexpected token reading atom: " ^ (tokenString token))
    | NONE => raise SyntaxError "end of input reached when reading atom"

and readForm r = case peek r of
    SOME PAREN_LEFT     => readList [] (rest r)
    | SOME BRACKET_LEFT => readVector [] (rest r)
    | SOME BRACE_LEFT   => readMap [] (rest r)
    | SOME AT           => let val (a, r') = readAtom (rest r) in (malList [SYMBOL "deref", a], r') end
    | SOME QUOTE        => let val (a, r') = readForm (rest r) in (malList [SYMBOL "quote", a], r') end
    | SOME BACK_TICK    => let val (a, r') = readForm (rest r) in (malList [SYMBOL "quasiquote", a], r') end
    | SOME TILDE        => let val (a, r') = readForm (rest r) in (malList [SYMBOL "unquote", a], r') end
    | SOME TILDE_AT     => let val (a, r') = readForm (rest r) in (malList [SYMBOL "splice-unquote", a], r') end
    | _                 => readAtom r

and readWithMeta r =
    let val (m, r')  = readForm r
        val (v, r'') = readForm r'
    in
        (malList [SYMBOL "with-meta", v, m], r'')
    end

and readList acc r =
    if peek r = SOME PAREN_RIGHT
    then (LIST (rev acc, NO_META), (rest r))
    else let val (a, r') = readForm r in readList (a::acc) r' end

and readVector acc r =
    if peek r = SOME BRACKET_RIGHT
    then (VECTOR (rev acc, NO_META), (rest r))
    else let val (a, r') = readForm r in readVector (a::acc) r' end

and readMap acc r =
    if peek r = SOME BRACE_RIGHT
    then (MAP (rev acc, NO_META), (rest r))
    else let val (k, r') = readForm r val (v, r'') = readForm r' in readMap (malAssoc acc k v) r'' end

fun clean ts =
    ts |> List.filter (fn x => x <> SPACE)
       |> List.filter (fn COMMENT _ => false | _ => true)

fun readStr s = case tokenize s |> clean of
    []   => raise Nothing
    | ts => ts |> READER |> readForm |> #1
