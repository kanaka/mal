MalTypes := Object clone

nil malPrint := method(readable, self asString)
true malPrint := method(readable, self asString)
false malPrint := method(readable, self asString)
Number malPrint := method(readable, self asString)

// Io strings are of type Sequence
Sequence malPrint := method(readable,
    if(readable,
        "\"" .. (self asString) .. "\"",
        self asString
    )
)

MalSymbol := Object clone do (
    val ::= nil
    with := method(str,
        self clone setVal(str)
    )
    malPrint := method(readable, val)
)

MalKeyword := Object clone do (
    val ::= nil
    with := method(str,
        self clone setVal(str)
    )
    malPrint := method(readable, ":" .. val)
)

MalList := List clone do (
    with := method(lst,
        self clone copy(lst)
    )
    malPrint := method(readable,
        "(" ..  (self map(e, e malPrint(readable)) join(" ")) .. ")"
    )
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
