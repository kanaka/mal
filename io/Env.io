Env := Object clone do(
    outer ::= nil
    data ::= nil

    with := method(envOuter,
        self clone setOuter(envOuter) setData(Map clone)
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
