MalCore := Object clone do(
    slurp := block(a,
        f := File with(a at(0))
        res := f contents
        f close
        res
    )

    swapBang := block(a,
        atom := a at(0)
        f := a at(1)
        args := MalList with(list(atom val)) appendSeq(a slice(2))
        newVal := f type switch(
            "Block", f call(args),
            "MalFunc", f blk call(args),
            Exception raise("Unknown function type")
        )
        atom setVal(newVal)
        newVal
    )

    NS := Map with(
        "=", block(a, a at(0) == a at(1)),

        "pr-str",  block(a, a map(s, s malPrint(true)) join(" ")),
        "str",     block(a, a map(s, s malPrint(false)) join("")),
        "prn",     block(a, a map(s, s malPrint(true)) join(" ") println ; nil),
        "println", block(a, a map(s, s malPrint(false)) join(" ") println ; nil),
        "read-string", block(a, MalReader read_str(a at(0))),
        "slurp",   slurp,

        "<",  block(a, a at(0) <  a at(1)),
        "<=", block(a, a at(0) <= a at(1)),
        ">",  block(a, a at(0) >  a at(1)),
        ">=", block(a, a at(0) >= a at(1)),
        "+",  block(a, a at(0) +  a at(1)),
        "-",  block(a, a at(0) -  a at(1)),
        "*",  block(a, a at(0) *  a at(1)),
        "/",  block(a, a at(0) /  a at(1)),

        "list",  block(a, a),
        "list?", block(a, a at(0) type == "MalList"),

        "cons",   block(a, MalList with(list(a at(0)) appendSeq(a at(1)))),
        "concat", block(a, MalList with(a reduce(appendSeq, list()))),
        "empty?", block(a, a at(0) ifNil(true) isEmpty),
        "count",  block(a, a at(0) ifNil(return(0)) size),

        "atom",   block(a, MalAtom with(a at(0))),
        "atom?",  block(a, a at(0) type == "MalAtom"),
        "deref",  block(a, a at(0) val),
        "reset!", block(a, a at(0) setVal(a at(1)) ; a at(1)),
        "swap!",  swapBang
    )
)
