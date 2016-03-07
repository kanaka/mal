MalCore := Object clone do(
    slurp := block(a,
        f := File with(a at(0))
        res := f contents
        f close
        res
    )

    dissoc := block(a,
        res := MalMap withMap(a at(0))
        a rest foreach(k, res removeKey(k))
        res
    )

    nth := block(a,
        if(a at(1) < a at(0) size,
            a at(0) at(a at(1)),
            Exception raise("nth: index out of range")
        )
    )

    conj := block(a,
        coll := a at(0)
        coll type switch(
            "MalList",
                MalList with(a rest reverse appendSeq(coll)),
            "MalVector",
                MalVector with(coll appendSeq(a rest))
        )
    )

    seq := block(a,
        obj := a at(0)
        (obj isNil) ifTrue(return(nil))
        (obj type == "MalList") ifTrue(return(if(obj isEmpty, nil, obj)))
        (obj type == "MalVector") ifTrue(return(if(obj isEmpty, nil, MalList with(obj))))
        (obj type == "Sequence") ifTrue(
            if(obj isEmpty, return(nil))
            lst := list()
            obj foreach(i, c, lst append(obj inclusiveSlice(i, i)))
            return(MalList with(lst))
        )
        nil
    )

    swapBang := block(a,
        atom := a at(0)
        newVal := a at(1) call(MalList with(list(atom val)) appendSeq(a slice(2)))
        atom setVal(newVal) val
    )

    NS := Map with(
        "=",     block(a, a at(0) == a at(1)),
        "throw", block(a, MalException with(a at(0)) raise),

        "nil?",     block(a, a at(0) isNil),
        "true?",    block(a, a at(0) == true),
        "false?",   block(a, a at(0) == false),
        "string?",  block(a, a at(0) type == "Sequence"),
        "symbol",   block(a, MalSymbol with(a at(0))),
        "symbol?",  block(a, a at(0) type == "MalSymbol"),
        "keyword",  block(a, MalKeyword with(a at(0))),
        "keyword?", block(a, a at(0) type == "MalKeyword"),

        "pr-str",  block(a, a map(s, s malPrint(true)) join(" ")),
        "str",     block(a, a map(s, s malPrint(false)) join("")),
        "prn",     block(a, a map(s, s malPrint(true)) join(" ") println ; nil),
        "println", block(a, a map(s, s malPrint(false)) join(" ") println ; nil),
        "read-string", block(a, MalReader read_str(a at(0))),
        "readline",    block(a, MalReadline readLine(a at(0))),
        "slurp",   slurp,

        "<",  block(a, a at(0) <  a at(1)),
        "<=", block(a, a at(0) <= a at(1)),
        ">",  block(a, a at(0) >  a at(1)),
        ">=", block(a, a at(0) >= a at(1)),
        "+",  block(a, a at(0) +  a at(1)),
        "-",  block(a, a at(0) -  a at(1)),
        "*",  block(a, a at(0) *  a at(1)),
        "/",  block(a, a at(0) /  a at(1)),
        "time-ms", block(a, (Date now asNumber * 1000.0) round),

        "list",      block(a, a),
        "list?",     block(a, a at(0) type == "MalList"),
        "vector",    block(a, MalVector with(a)),
        "vector?",   block(a, a at(0) type == "MalVector"),
        "hash-map",  block(a, MalMap withList(a)),
        "map?",      block(a, a at(0) type == "MalMap"),
        "assoc",     block(a, MalMap withMap(a at(0) merge(MalMap withList(a rest)))),
        "dissoc",    dissoc,
        "get",       block(a, a at(0) ifNil(return nil) get(a at(1))),
        "contains?", block(a, a at(0) ifNil(return nil) contains(a at(1))),
        "keys",      block(a, a at(0) malKeys),
        "vals",      block(a, a at(0) malVals),

        "sequential?", block(a, if(a at(0) ?isSequential, true, false)),
        "cons",   block(a, MalList with(list(a at(0)) appendSeq(a at(1)))),
        "concat", block(a, MalList with(a reduce(appendSeq, list()))),
        "nth",    nth,
        "first",  block(a, a at(0) ifNil(return nil) first),
        "rest",   block(a, a at(0) ifNil(return MalList with(list())) rest),
        "empty?", block(a, a at(0) ifNil(true) isEmpty),
        "count",  block(a, a at(0) ifNil(return(0)) size),
        "apply",  block(a, a at(0) call(MalList with(a slice(1, -1) appendSeq(a last)))),
        "map",    block(a, MalList with(a at(1) map(e, a at(0) call(MalList with(list(e)))))),

        "conj",   conj,
        "seq",    seq,

        "meta",      block(a, a at(0) ?meta),
        "with-meta", block(a, a at(0) clone setMeta(a at(1))),
        "atom",      block(a, MalAtom with(a at(0))),
        "atom?",     block(a, a at(0) type == "MalAtom"),
        "deref",     block(a, a at(0) val),
        "reset!",    block(a, a at(0) setVal(a at(1)) ; a at(1)),
        "swap!",     swapBang
    )
)
