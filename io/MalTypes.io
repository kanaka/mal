MalTypes := Object clone

nil malPrint := method(readable, self asString)
true malPrint := method(readable, self asString)
false malPrint := method(readable, self asString)
Number malPrint := method(readable, self asString)

// Io strings are of type Sequence
Sequence malPrint := method(readable,
    if(readable, self asString asJson, self asString)
)

MalMeta := Object clone do(
    meta ::= nil
)

MalSymbol := Object clone appendProto(MalMeta) do (
    val ::= nil
    with := method(str, self clone setVal(str))
    malPrint := method(readable, val)
    == := method(other, (self type == other type) and (val == other val))
)

MalKeyword := Object clone do (
    val ::= nil
    with := method(str, self clone setVal(str))
    malPrint := method(readable, ":" .. val)
    == := method(other, (self type == other type) and (val == other val))
)

MalSequential := Object clone do(
    isSequential := method(true)
)

MalList := List clone appendProto(MalSequential) appendProto(MalMeta) do (
    with := method(lst, self clone copy(lst))
    malPrint := method(readable,
        "(" ..  (self map(e, e malPrint(readable)) join(" ")) .. ")"
    )
    rest := method(MalList with(resend))
    slice := method(MalList with(resend))
)

MalVector := List clone appendProto(MalSequential) appendProto(MalMeta) do (
    with := method(lst, self clone copy(lst))
    malPrint := method(readable,
        "[" ..  (self map(e, e malPrint(readable)) join(" ")) .. "]"
    )
    rest := method(MalList with(resend))
    slice := method(MalList with(resend))
)

MalMap := Map clone appendProto(MalMeta) do (
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
    withMap := method(aMap, self clone merge(aMap))
    objToKey := method(obj,
        if(obj type == "MalKeyword", "K_" .. (obj val), "S_" .. obj)
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
    contains := method(obj, hasKey(objToKey(obj)))
    get := method(obj, at(objToKey(obj)))
    malKeys := method(MalList with(keys map(k, keyToObj(k))))
    malVals := method(MalList with(values))
    removeKey := method(obj, removeAt(objToKey(obj)))
    == := method(other,
        if(self type != other type, return false)
        if(keys size != other keys size, return false)
        unequalElement := self detect(k, valA,
            (valA == (other at(k))) not
        )
        if(unequalElement, false, true)
    )
)

Block malPrint := method(readable, "#<NativeFunction>")
Block appendProto(MalMeta)

MalFunc := Object clone appendProto(MalMeta) do (
    ast ::= nil
    params ::= nil
    env ::= nil
    blk ::= nil
    isMacro ::= false
    with := method(aAst, aParams, aEnv, aBlk,
        self clone setAst(aAst) setParams(aParams) setEnv(aEnv) setBlk(aBlk)
    )
    malPrint := method(readable, "#<Function:params=" ..  (params malPrint(true)) .. ">")
    call := method(args, blk call(args))
)

MalAtom := Object clone do (
    val ::= nil
    with := method(str, self clone setVal(str))
    malPrint := method(readable, "(atom " .. (val malPrint(true)) .. ")")
    == := method(other, (self type == other type) and (val == other val))
)

MalException := Exception clone do (
    val ::= nil
    with := method(str, self clone setVal(str))
)
