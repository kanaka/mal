package main

import (
	"errors"
	"fmt"
	"os"
	"strings"
)

import (
	"mal/src/core"
	. "mal/src/env"
	"mal/src/printer"
	"mal/src/reader"
	"mal/src/readline"
	. "mal/src/types"
)

// read
func READ(str string) (MalType, error) {
	return reader.Read_str(str)
}

// eval
func starts_with(xs []MalType, sym string) bool {
	if 0 < len(xs) {
		switch s := xs[0].(type) {
		case Symbol:
			return s.Val == sym
		default:
		}
	}
	return false
}

func qq_loop(xs []MalType) MalType {
	acc := NewList()
	for i := len(xs) - 1; 0<=i; i -= 1 {
		elt := xs[i]
		switch e := elt.(type) {
		case List:
			if starts_with(e.Val, "splice-unquote") {
				acc = NewList(Symbol{"concat"}, e.Val[1], acc)
				continue
			}
		default:
		}
		acc = NewList(Symbol{"cons"}, quasiquote(elt), acc)
	}
	return acc
}

func quasiquote(ast MalType) MalType {
	switch a := ast.(type) {
	case Vector:
		return NewList(Symbol{"vec"}, qq_loop(a.Val))
	case HashMap, Symbol:
		return NewList(Symbol{"quote"}, ast)
	case List:
		if starts_with(a.Val,"unquote") {
			return a.Val[1]
		} else {
			return qq_loop(a.Val)
		}
	default:
		return ast
	}
}

func map_eval(xs []MalType, env EnvType) ([]MalType, error) {
	lst := []MalType{}
	for _, a := range xs {
		exp, e := EVAL(a, env)
		if e != nil {
			return nil, e
		}
		lst = append(lst, exp)
	}
	return lst, nil
}

func EVAL(ast MalType, env EnvType) (MalType, error) {
	for {
	//fmt.Printf("EVAL: %v\n", printer.Pr_str(ast, true))

	if Symbol_Q(ast) {
		return env.Get(ast.(Symbol))
	} else if Vector_Q(ast) {
		lst, e := map_eval(ast.(Vector).Val, env)
		if e != nil {
			return nil, e
		}
		return Vector{lst, nil}, nil
	} else if HashMap_Q(ast) {
		m := ast.(HashMap)
		new_hm := HashMap{map[string]MalType{}, nil}
		for k, v := range m.Val {
			kv, e2 := EVAL(v, env)
			if e2 != nil {
				return nil, e2
			}
			new_hm.Val[k] = kv
		}
		return new_hm, nil
	} else if !List_Q(ast) {
		return ast, nil
	} else {
		// apply list
		if len(ast.(List).Val) == 0 {
			return ast, nil
		}

		a0 := ast.(List).Val[0]
		var a1 MalType = nil
		var a2 MalType = nil
		switch len(ast.(List).Val) {
		case 1:
			a1 = nil
			a2 = nil
		case 2:
			a1 = ast.(List).Val[1]
			a2 = nil
		default:
			a1 = ast.(List).Val[1]
			a2 = ast.(List).Val[2]
		}
		a0sym := "__<*fn*>__"
		if Symbol_Q(a0) {
			a0sym = a0.(Symbol).Val
		}
		switch a0sym {
		case "def!":
			res, e := EVAL(a2, env)
			if e != nil {
				return nil, e
			}
			return env.Set(a1.(Symbol), res), nil
		case "let*":
			let_env, e := NewEnv(env, nil, nil)
			if e != nil {
				return nil, e
			}
			arr1, e := GetSlice(a1)
			if e != nil {
				return nil, e
			}
			for i := 0; i < len(arr1); i += 2 {
				if !Symbol_Q(arr1[i]) {
					return nil, errors.New("non-symbol bind value")
				}
				exp, e := EVAL(arr1[i+1], let_env)
				if e != nil {
					return nil, e
				}
				let_env.Set(arr1[i].(Symbol), exp)
			}
			ast = a2
			env = let_env
		case "quote":
			return a1, nil
		case "quasiquote":
			ast = quasiquote(a1)
		case "defmacro!":
			fn, e := EVAL(a2, env)
			fn = fn.(MalFunc).SetMacro()
			if e != nil {
				return nil, e
			}
			return env.Set(a1.(Symbol), fn), nil
		case "try*":
			var exc MalType
			exp, e := EVAL(a1, env)
			if e == nil {
				return exp, nil
			} else {
				if a2 != nil && List_Q(a2) {
					a2s, _ := GetSlice(a2)
					if Symbol_Q(a2s[0]) && (a2s[0].(Symbol).Val == "catch*") {
						switch e.(type) {
						case MalError:
							exc = e.(MalError).Obj
						default:
							exc = e.Error()
						}
						binds := NewList(a2s[1])
						new_env, e := NewEnv(env, binds, NewList(exc))
						if e != nil {
							return nil, e
						}
						exp, e = EVAL(a2s[2], new_env)
						if e == nil {
							return exp, nil
						}
					}
				}
				return nil, e
			}
		case "do":
			lst := ast.(List).Val
			_, e := map_eval(lst[1 : len(lst)-1], env)
			if e != nil {
				return nil, e
			}
			if len(lst) == 1 {
				return nil, nil
			}
			ast = lst[len(lst)-1]
		case "if":
			cond, e := EVAL(a1, env)
			if e != nil {
				return nil, e
			}
			if cond == nil || cond == false {
				if len(ast.(List).Val) >= 4 {
					ast = ast.(List).Val[3]
				} else {
					return nil, nil
				}
			} else {
				ast = a2
			}
		case "fn*":
			fn := MalFunc{EVAL, a2, env, a1, false, NewEnv, nil}
			return fn, nil
		default:
			f, e := EVAL(a0, env)
			if e != nil {
				return nil, e
			}
			args := ast.(List).Val[1:]
			if MalFunc_Q(f) && f.(MalFunc).GetMacro() {
				new_ast, e := Apply(f.(MalFunc), args)
				if e != nil {
					return nil, e
				}
				ast = new_ast
				continue
			}
			args, e = map_eval(args, env)
			if e != nil {
				return nil, e
			}
			if MalFunc_Q(f) {
				fn := f.(MalFunc)
				ast = fn.Exp
				env, e = NewEnv(fn.Env, fn.Params, List{args, nil})
				if e != nil {
					return nil, e
				}
			} else {
				fn, ok := f.(Func)
				if !ok {
					return nil, errors.New("attempt to call non-function")
				}
				return fn.Fn(args)
			}
		}
	}
	} // TCO loop
}

// print
func PRINT(exp MalType) (string, error) {
	return printer.Pr_str(exp, true), nil
}

var repl_env, _ = NewEnv(nil, nil, nil)

// repl
func rep(str string) (MalType, error) {
	var exp MalType
	var res string
	var e error
	if exp, e = READ(str); e != nil {
		return nil, e
	}
	if exp, e = EVAL(exp, repl_env); e != nil {
		return nil, e
	}
	if res, e = PRINT(exp); e != nil {
		return nil, e
	}
	return res, nil
}

func main() {
	// core.go: defined using go
	for k, v := range core.NS {
		repl_env.Set(Symbol{k}, Func{v.(func([]MalType) (MalType, error)), nil})
	}
	repl_env.Set(Symbol{"eval"}, Func{func(a []MalType) (MalType, error) {
		return EVAL(a[0], repl_env)
	}, nil})
	repl_env.Set(Symbol{"*ARGV*"}, List{})

	// core.mal: defined using the language itself
	rep("(def! *host-language* \"go\")")
	rep("(def! not (fn* (a) (if a false true)))")
	rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
	rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

	// called with mal script to load and eval
	if len(os.Args) > 1 {
		args := make([]MalType, 0, len(os.Args)-2)
		for _, a := range os.Args[2:] {
			args = append(args, a)
		}
		repl_env.Set(Symbol{"*ARGV*"}, List{args, nil})
		if _, e := rep("(load-file \"" + os.Args[1] + "\")"); e != nil {
			fmt.Printf("Error: %v\n", e)
			os.Exit(1)
		}
		os.Exit(0)
	}

	// repl loop
	rep("(println (str \"Mal [\" *host-language* \"]\"))")
	for {
		text, err := readline.Readline("user> ")
		text = strings.TrimRight(text, "\n")
		if err != nil {
			return
		}
		var out MalType
		var e error
		if out, e = rep(text); e != nil {
			if e.Error() == "<empty line>" {
				continue
			}
			fmt.Printf("Error: %v\n", e)
			continue
		}
		fmt.Printf("%v\n", out)
	}
}
