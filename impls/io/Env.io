Env := Object clone do(
    outer ::= nil
    data ::= nil

    with := method(aOuter, aBinds, aExprs,
        self clone setOuter(aOuter) setData(Map clone) initBinds(aBinds, aExprs)
    )

    initBinds := method(aBinds, aExprs,
        if(aBinds isNil not,
            aBinds foreach(i, b,
                if(b val == "&",
                    set(aBinds at(i + 1), aExprs slice(i)) break,
                    set(b, aExprs at(i))
                )
            )
        )
        self
    )

    set := method(key, val,
        data atPut(key val, val)
        val
    )

    find := method(key,
        keyStr := key val
        if(data hasKey(keyStr),
            self,
            if(outer isNil,
                nil,
                outer find(key)
            )
        )
    )

    get := method(key,
        keyStr := key val
        foundEnv := find(key)
        if(foundEnv isNil,
            Exception raise("'" .. keyStr .. "' not found"),
            (foundEnv data) at(keyStr)
        )
    )
)
