MalReader := Object clone do (

    Reader := Object clone do (
        pos ::= 0
        tokens ::= list()

        with := method(theTokens,
            self clone setTokens(theTokens)
        )

        peek := method(tokens at(pos))

        next := method(
            pos = pos + 1
            tokens at(pos - 1)
        )
    )

    tokenizerRegex := Regex with("[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)")

    tokenize := method(str,
        tokenizerRegex matchesIn(str) \
            map(m, m at(1) asMutable strip) \
            select(t, t size > 0) \
            select(t, t exSlice(0, 1) != ";")
    )

    numberRegex := Regex with("^-?[0-9]+$")

    read_string := method(token,
        token exSlice(1, -1) replaceSeq("\\\"", "\"") replaceSeq("\\n", "\n") replaceSeq("\\\\", "\\")
    )

    read_atom := method(rdr,
        token := rdr next
        (token hasMatchOfRegex(numberRegex)) ifTrue(return(token asNumber))
        (token == "true") ifTrue(return(true))
        (token == "false") ifTrue(return(false))
        (token == "nil") ifTrue(return(nil))
        (token beginsWithSeq(":")) ifTrue(return(MalKeyword with(token exSlice(1))))
        (token beginsWithSeq("\"")) ifTrue(return(read_string(token)))
        MalSymbol with(token)
    )

    read_list := method(rdr, start, end,
        token := rdr next
        if(token != start, Exception raise("expected '" .. start .. "'"))
        ast := list()
        token = rdr peek
        while(token != end,
            if(token isNil, Exception raise("expected '" .. end .. "', got EOF"))
            ast push(read_form(rdr))
            token = rdr peek
        )
        rdr next
        ast
    )

    reader_macro := method(symbol, rdr,
        rdr next
        MalList with(list(MalSymbol with(symbol), read_form(rdr)))
    )

    read_form := method(rdr,
        token := rdr peek
        (token == "'") ifTrue(return(reader_macro("quote", rdr)))
        (token == "`") ifTrue(return(reader_macro("quasiquote", rdr)))
        (token == "~") ifTrue(return(reader_macro("unquote", rdr)))
        (token == "~@") ifTrue(return(reader_macro("splice-unquote", rdr)))
        (token == "^") ifTrue(
            rdr next
            meta := read_form(rdr)
            return(MalList with(list(MalSymbol with("with-meta"), read_form(rdr), meta)))
        )
        (token == "@") ifTrue(return(reader_macro("deref", rdr)))
        (token == "(") ifTrue(return(MalList with(read_list(rdr, "(", ")"))))
        (token == ")") ifTrue(Exception raise("unexepcted ')'"))
        (token == "[") ifTrue(return(MalVector with(read_list(rdr, "[", "]"))))
        (token == "]") ifTrue(Exception raise("unexepcted ']'"))
        (token == "{") ifTrue(return(MalMap withList(read_list(rdr, "{", "}"))))
        (token == "}") ifTrue(Exception raise("unexepcted '}'"))
        read_atom(rdr)
    )

    read_str := method(str,
        tokens := tokenize(str)
        if(tokens isEmpty, nil, read_form(Reader with(tokens)))
    )
)
