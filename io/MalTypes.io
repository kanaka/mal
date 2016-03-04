MalTypes := Object clone

nil malPrint := method(readable, self asString)
true malPrint := method(readable, self asString)
false malPrint := method(readable, self asString)
Number malPrint := method(readable, self asString)

// Io strings are of type Sequence
Sequence malPrint := method(readable,
    if(readable, self asString asJson, self asString)
)

MalSymbol := Object clone do (
    val ::= nil
    with := method(str,
        self clone setVal(str)
    )
    malPrint := method(readable, val)
    == := method(other, other isKindOf(MalSymbol) and (val == other val))
)

MalKeyword := Object clone do (
    val ::= nil
    with := method(str,
        self clone setVal(str)
    )
    malPrint := method(readable, ":" .. val)
    == := method(other, other isKindOf(MalKeyword) and (val == other val))
)

MalList := List clone do (
    with := method(lst,
        self clone copy(lst)
    )
    malPrint := method(readable,
        "(" ..  (self map(e, e malPrint(readable)) join(" ")) .. ")"
    )
    rest := method(MalList with(resend))
    slice := method(MalList with(resend))
)

MalVector := List clone do (
    with := method(lst,
        self clone copy(lst)
    )
    malPrint := method(readable,
        "[" ..  (self map(e, e malPrint(readable)) join(" ")) .. "]"
    )
)

MalMap := Map clone do (
    withList := method(lst,
        obj := self clone
        k := nil
        lst foreach(i, e,
            if(i % 2 == 0,
                k := e,
                obj atPut(objToKey(k), e)
            )
        )
        obj
    )
    objToKey := method(obj,
        if(obj isKindOf(MalKeyword), "K_" .. (obj val), "S_" .. obj)
    )
    keyToObj := method(s,
        if(s beginsWithSeq("K_"),
            MalKeyword with(s exSlice(2)),
            s exSlice(2)
        )
    )
    malPrint := method(readable,
        "{" ..
            (self map(k, v,
                (keyToObj(k) malPrint(readable)) .. " " .. (v malPrint(readable))
            ) join(" ")) .. "}"
    )
)

Block malPrint := method(readable, "#<NativeFunction>")

MalFunc := Object clone do (
    ast ::= nil
    params ::= nil
    env ::= nil
    blk ::= nil

    with := method(aAst, aParams, aEnv, aBlk,
        self clone setAst(aAst) setParams(aParams) setEnv(aEnv) setBlk(aBlk)
    )
    malPrint := method(readable, "#<Function:params=" ..  (params malPrint(true)) .. ">")
)

MalAtom := Object clone do (
    val ::= nil
    with := method(str,
        self clone setVal(str)
    )
    malPrint := method(readable, "(atom " .. (val malPrint(true)) .. ")")
    == := method(other, other isKindOf(MalAtom) and (val == other val))
)
