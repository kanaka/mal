package env

import (
    "errors"
    //"fmt"
)

import (
    . "types"
)

type Env struct {
    data map[string]MalType
    outer EnvType
}

func NewEnv(outer EnvType, binds_mt MalType, exprs_mt MalType) (EnvType, error) {
    env := Env{map[string]MalType{}, outer}

    if binds_mt != nil && exprs_mt != nil {
        binds, e := GetSlice(binds_mt); if e != nil { return nil, e }
        exprs, e := GetSlice(exprs_mt); if e != nil { return nil, e }
        // Return a new Env with symbols in binds boudn to
        // corresponding values in exprs
        for i := 0; i < len(binds); i+=1 {
            if (Symbol_Q(binds[i]) && binds[i].(Symbol).Val == "&") {
                env.data[binds[i+1].(Symbol).Val] = List{exprs[i:],nil}
                break
            } else {
                env.data[binds[i].(Symbol).Val] = exprs[i]
            }
        }
    }
    //return &et, nil
    return env, nil
}

func (e Env) Find(key Symbol) EnvType {
    if _, ok := e.data[key.Val]; ok {
        return e
    } else if (e.outer != nil) {
        return e.outer.Find(key)
    } else {
        return nil
    }
}

func (e Env) Set(key Symbol, value MalType) MalType {
    e.data[key.Val] = value
    return value
}

func (e Env) Get(key Symbol) (MalType, error) {
    env := e.Find(key)
    if env == nil {
        return nil, errors.New("'" + key.Val + "' not found")
    }
    return env.(Env).data[key.Val], nil
}
