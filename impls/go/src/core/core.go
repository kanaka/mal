package core

import (
	"errors"
	"fmt"
	"io/ioutil"
	"strings"
	"time"
)

import (
	"printer"
	"reader"
	"readline"
	. "types"
)

// Errors/Exceptions
func throw(a []MalType) (MalType, error) {
	return nil, MalError{a[0]}
}

func fn_q(a []MalType) (MalType, error) {
	switch f := a[0].(type) {
	case MalFunc:
		return !f.GetMacro(), nil
	case Func:
		return true, nil
	case func([]MalType) (MalType, error):
		return true, nil
	default:
		return false, nil
	}
}

// String functions

func pr_str(a []MalType) (MalType, error) {
	return printer.Pr_list(a, true, "", "", " "), nil
}

func str(a []MalType) (MalType, error) {
	return printer.Pr_list(a, false, "", "", ""), nil
}

func prn(a []MalType) (MalType, error) {
	fmt.Println(printer.Pr_list(a, true, "", "", " "))
	return nil, nil
}

func println(a []MalType) (MalType, error) {
	fmt.Println(printer.Pr_list(a, false, "", "", " "))
	return nil, nil
}

func slurp(a []MalType) (MalType, error) {
	b, e := ioutil.ReadFile(a[0].(string))
	if e != nil {
		return nil, e
	}
	return string(b), nil
}

// Number functions
func time_ms(a []MalType) (MalType, error) {
	return int(time.Now().UnixNano() / int64(time.Millisecond)), nil
}

// Hash Map functions
func copy_hash_map(hm HashMap) HashMap {
	new_hm := HashMap{map[string]MalType{}, nil}
	for k, v := range hm.Val {
		new_hm.Val[k] = v
	}
	return new_hm
}

func assoc(a []MalType) (MalType, error) {
	if len(a) < 3 {
		return nil, errors.New("assoc requires at least 3 arguments")
	}
	if len(a)%2 != 1 {
		return nil, errors.New("assoc requires odd number of arguments")
	}
	if !HashMap_Q(a[0]) {
		return nil, errors.New("assoc called on non-hash map")
	}
	new_hm := copy_hash_map(a[0].(HashMap))
	for i := 1; i < len(a); i += 2 {
		key := a[i]
		if !String_Q(key) {
			return nil, errors.New("assoc called with non-string key")
		}
		new_hm.Val[key.(string)] = a[i+1]
	}
	return new_hm, nil
}

func dissoc(a []MalType) (MalType, error) {
	if len(a) < 2 {
		return nil, errors.New("dissoc requires at least 3 arguments")
	}
	if !HashMap_Q(a[0]) {
		return nil, errors.New("dissoc called on non-hash map")
	}
	new_hm := copy_hash_map(a[0].(HashMap))
	for i := 1; i < len(a); i += 1 {
		key := a[i]
		if !String_Q(key) {
			return nil, errors.New("dissoc called with non-string key")
		}
		delete(new_hm.Val, key.(string))
	}
	return new_hm, nil
}

func get(a []MalType) (MalType, error) {
	if Nil_Q(a[0]) {
		return nil, nil
	}
	if !HashMap_Q(a[0]) {
		return nil, errors.New("get called on non-hash map")
	}
	if !String_Q(a[1]) {
		return nil, errors.New("get called with non-string key")
	}
	return a[0].(HashMap).Val[a[1].(string)], nil
}

func contains_Q(hm MalType, key MalType) (MalType, error) {
	if Nil_Q(hm) {
		return false, nil
	}
	if !HashMap_Q(hm) {
		return nil, errors.New("get called on non-hash map")
	}
	if !String_Q(key) {
		return nil, errors.New("get called with non-string key")
	}
	_, ok := hm.(HashMap).Val[key.(string)]
	return ok, nil
}

func keys(a []MalType) (MalType, error) {
	if !HashMap_Q(a[0]) {
		return nil, errors.New("keys called on non-hash map")
	}
	slc := []MalType{}
	for k, _ := range a[0].(HashMap).Val {
		slc = append(slc, k)
	}
	return List{slc, nil}, nil
}

func vals(a []MalType) (MalType, error) {
	if !HashMap_Q(a[0]) {
		return nil, errors.New("keys called on non-hash map")
	}
	slc := []MalType{}
	for _, v := range a[0].(HashMap).Val {
		slc = append(slc, v)
	}
	return List{slc, nil}, nil
}

// Sequence functions

func cons(a []MalType) (MalType, error) {
	val := a[0]
	lst, e := GetSlice(a[1])
	if e != nil {
		return nil, e
	}
	return List{append([]MalType{val}, lst...), nil}, nil
}

func concat(a []MalType) (MalType, error) {
	if len(a) == 0 {
		return List{}, nil
	}
	slc1, e := GetSlice(a[0])
	if e != nil {
		return nil, e
	}
	for i := 1; i < len(a); i += 1 {
		slc2, e := GetSlice(a[i])
		if e != nil {
			return nil, e
		}
		slc1 = append(slc1, slc2...)
	}
	return List{slc1, nil}, nil
}

func vec(a []MalType) (MalType, error) {
	switch obj := a[0].(type) {
	case Vector:
		return obj, nil
	case List:
		return Vector{obj.Val, nil}, nil
	default:
		return nil, errors.New("vec: expects a sequence")
	}
}

func nth(a []MalType) (MalType, error) {
	slc, e := GetSlice(a[0])
	if e != nil {
		return nil, e
	}
	idx := a[1].(int)
	if idx < len(slc) {
		return slc[idx], nil
	} else {
		return nil, errors.New("nth: index out of range")
	}
}

func first(a []MalType) (MalType, error) {
	if len(a) == 0 {
		return nil, nil
	}
	if a[0] == nil {
		return nil, nil
	}
	slc, e := GetSlice(a[0])
	if e != nil {
		return nil, e
	}
	if len(slc) == 0 {
		return nil, nil
	}
	return slc[0], nil
}

func rest(a []MalType) (MalType, error) {
	if a[0] == nil {
		return List{}, nil
	}
	slc, e := GetSlice(a[0])
	if e != nil {
		return nil, e
	}
	if len(slc) == 0 {
		return List{}, nil
	}
	return List{slc[1:], nil}, nil
}

func empty_Q(a []MalType) (MalType, error) {
	switch obj := a[0].(type) {
	case List:
		return len(obj.Val) == 0, nil
	case Vector:
		return len(obj.Val) == 0, nil
	case nil:
		return true, nil
	default:
		return nil, errors.New("empty? called on non-sequence")
	}
}

func count(a []MalType) (MalType, error) {
	switch obj := a[0].(type) {
	case List:
		return len(obj.Val), nil
	case Vector:
		return len(obj.Val), nil
	case map[string]MalType:
		return len(obj), nil
	case nil:
		return 0, nil
	default:
		return nil, errors.New("count called on non-sequence")
	}
}

func apply(a []MalType) (MalType, error) {
	if len(a) < 2 {
		return nil, errors.New("apply requires at least 2 args")
	}
	f := a[0]
	args := []MalType{}
	for _, b := range a[1 : len(a)-1] {
		args = append(args, b)
	}
	last, e := GetSlice(a[len(a)-1])
	if e != nil {
		return nil, e
	}
	args = append(args, last...)
	return Apply(f, args)
}

func do_map(a []MalType) (MalType, error) {
	f := a[0]
	results := []MalType{}
	args, e := GetSlice(a[1])
	if e != nil {
		return nil, e
	}
	for _, arg := range args {
		res, e := Apply(f, []MalType{arg})
		results = append(results, res)
		if e != nil {
			return nil, e
		}
	}
	return List{results, nil}, nil
}

func conj(a []MalType) (MalType, error) {
	if len(a) < 2 {
		return nil, errors.New("conj requires at least 2 arguments")
	}
	switch seq := a[0].(type) {
	case List:
		new_slc := []MalType{}
		for i := len(a) - 1; i > 0; i -= 1 {
			new_slc = append(new_slc, a[i])
		}
		return List{append(new_slc, seq.Val...), nil}, nil
	case Vector:
		new_slc := seq.Val
		for _, x := range a[1:] {
			new_slc = append(new_slc, x)
		}
		return Vector{new_slc, nil}, nil
	}

	if !HashMap_Q(a[0]) {
		return nil, errors.New("dissoc called on non-hash map")
	}
	new_hm := copy_hash_map(a[0].(HashMap))
	for i := 1; i < len(a); i += 1 {
		key := a[i]
		if !String_Q(key) {
			return nil, errors.New("dissoc called with non-string key")
		}
		delete(new_hm.Val, key.(string))
	}
	return new_hm, nil
}

func seq(a []MalType) (MalType, error) {
	if a[0] == nil {
		return nil, nil
	}
	switch arg := a[0].(type) {
	case List:
		if len(arg.Val) == 0 {
			return nil, nil
		}
		return arg, nil
	case Vector:
		if len(arg.Val) == 0 {
			return nil, nil
		}
		return List{arg.Val, nil}, nil
	case string:
		if len(arg) == 0 {
			return nil, nil
		}
		new_slc := []MalType{}
		for _, ch := range strings.Split(arg, "") {
			new_slc = append(new_slc, ch)
		}
		return List{new_slc, nil}, nil
	}
	return nil, errors.New("seq requires string or list or vector or nil")
}

// Metadata functions
func with_meta(a []MalType) (MalType, error) {
	obj := a[0]
	m := a[1]
	switch tobj := obj.(type) {
	case List:
		return List{tobj.Val, m}, nil
	case Vector:
		return Vector{tobj.Val, m}, nil
	case HashMap:
		return HashMap{tobj.Val, m}, nil
	case Func:
		return Func{tobj.Fn, m}, nil
	case MalFunc:
		fn := tobj
		fn.Meta = m
		return fn, nil
	default:
		return nil, errors.New("with-meta not supported on type")
	}
}

func meta(a []MalType) (MalType, error) {
	obj := a[0]
	switch tobj := obj.(type) {
	case List:
		return tobj.Meta, nil
	case Vector:
		return tobj.Meta, nil
	case HashMap:
		return tobj.Meta, nil
	case Func:
		return tobj.Meta, nil
	case MalFunc:
		return tobj.Meta, nil
	default:
		return nil, errors.New("meta not supported on type")
	}
}

// Atom functions
func deref(a []MalType) (MalType, error) {
	if !Atom_Q(a[0]) {
		return nil, errors.New("deref called with non-atom")
	}
	return a[0].(*Atom).Val, nil
}

func reset_BANG(a []MalType) (MalType, error) {
	if !Atom_Q(a[0]) {
		return nil, errors.New("reset! called with non-atom")
	}
	a[0].(*Atom).Set(a[1])
	return a[1], nil
}

func swap_BANG(a []MalType) (MalType, error) {
	if !Atom_Q(a[0]) {
		return nil, errors.New("swap! called with non-atom")
	}
	atm := a[0].(*Atom)
	args := []MalType{atm.Val}
	f := a[1]
	args = append(args, a[2:]...)
	res, e := Apply(f, args)
	if e != nil {
		return nil, e
	}
	atm.Set(res)
	return res, nil
}

// core namespace
var NS = map[string]MalType{
	"=":       call2b(Equal_Q),
	"throw":   call1e(throw),
	"nil?":    call1b(Nil_Q),
	"true?":   call1b(True_Q),
	"false?":  call1b(False_Q),
	"symbol":  call1e(func(a []MalType) (MalType, error) { return Symbol{a[0].(string)}, nil }),
	"symbol?": call1b(Symbol_Q),
	"string?": call1e(func(a []MalType) (MalType, error) { return (String_Q(a[0]) && !Keyword_Q(a[0])), nil }),
	"keyword": call1e(func(a []MalType) (MalType, error) {
		if Keyword_Q(a[0]) {
			return a[0], nil
		} else {
			return NewKeyword(a[0].(string))
		}
	}),
	"keyword?":    call1b(Keyword_Q),
	"number?":     call1b(Number_Q),
	"fn?":         call1e(fn_q),
	"macro?":      call1e(func(a []MalType) (MalType, error) { return MalFunc_Q(a[0]) && a[0].(MalFunc).GetMacro(), nil }),
	"pr-str":      callNe(pr_str),
	"str":         callNe(str),
	"prn":         callNe(prn),
	"println":     callNe(println),
	"read-string": call1e(func(a []MalType) (MalType, error) { return reader.Read_str(a[0].(string)) }),
	"slurp":       call1e(slurp),
	"readline":    call1e(func(a []MalType) (MalType, error) { return readline.Readline(a[0].(string)) }),
	"<":           call2e(func(a []MalType) (MalType, error) { return a[0].(int) < a[1].(int), nil }),
	"<=":          call2e(func(a []MalType) (MalType, error) { return a[0].(int) <= a[1].(int), nil }),
	">":           call2e(func(a []MalType) (MalType, error) { return a[0].(int) > a[1].(int), nil }),
	">=":          call2e(func(a []MalType) (MalType, error) { return a[0].(int) >= a[1].(int), nil }),
	"+":           call2e(func(a []MalType) (MalType, error) { return a[0].(int) + a[1].(int), nil }),
	"-":           call2e(func(a []MalType) (MalType, error) { return a[0].(int) - a[1].(int), nil }),
	"*":           call2e(func(a []MalType) (MalType, error) { return a[0].(int) * a[1].(int), nil }),
	"/":           call2e(func(a []MalType) (MalType, error) { return a[0].(int) / a[1].(int), nil }),
	"time-ms":     call0e(time_ms),
	"list":        callNe(func(a []MalType) (MalType, error) { return List{a, nil}, nil }),
	"list?":       call1b(List_Q),
	"vector":      callNe(func(a []MalType) (MalType, error) { return Vector{a, nil}, nil }),
	"vector?":     call1b(Vector_Q),
	"hash-map":    callNe(func(a []MalType) (MalType, error) { return NewHashMap(List{a, nil}) }),
	"map?":        call1b(HashMap_Q),
	"assoc":       callNe(assoc),  // at least 3
	"dissoc":      callNe(dissoc), // at least 2
	"get":         call2e(get),
	"contains?":   call2e(func(a []MalType) (MalType, error) { return contains_Q(a[0], a[1]) }),
	"keys":        call1e(keys),
	"vals":        call1e(vals),
	"sequential?": call1b(Sequential_Q),
	"cons":        call2e(cons),
	"concat":      callNe(concat),
	"vec":         call1e(vec),
	"nth":         call2e(nth),
	"first":       call1e(first),
	"rest":        call1e(rest),
	"empty?":      call1e(empty_Q),
	"count":       call1e(count),
	"apply":       callNe(apply), // at least 2
	"map":         call2e(do_map),
	"conj":        callNe(conj), // at least 2
	"seq":         call1e(seq),
	"with-meta":   call2e(with_meta),
	"meta":        call1e(meta),
	"atom":        call1e(func(a []MalType) (MalType, error) { return &Atom{a[0], nil}, nil }),
	"atom?":       call1b(Atom_Q),
	"deref":       call1e(deref),
	"reset!":      call2e(reset_BANG),
	"swap!":       callNe(swap_BANG),
}

// callXX functions check the number of arguments
func call0e(f func([]MalType) (MalType, error)) func([]MalType) (MalType, error) {
	return func(args []MalType) (MalType, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("wrong number of arguments (%d instead of 0)", len(args))
		}
		return f(args)
	}
}

func call1e(f func([]MalType) (MalType, error)) func([]MalType) (MalType, error) {
	return func(args []MalType) (MalType, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("wrong number of arguments (%d instead of 1)", len(args))
		}
		return f(args)
	}
}

func call2e(f func([]MalType) (MalType, error)) func([]MalType) (MalType, error) {
	return func(args []MalType) (MalType, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("wrong number of arguments (%d instead of 2)", len(args))
		}
		return f(args)
	}
}

func callNe(f func([]MalType) (MalType, error)) func([]MalType) (MalType, error) {
	// just for documenting purposes, does not check anything
	return func(args []MalType) (MalType, error) {
		return f(args)
	}
}

func call1b(f func(MalType) bool) func([]MalType) (MalType, error) {
	return func(args []MalType) (MalType, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("wrong number of arguments (%d instead of 1)", len(args))
		}
		return f(args[0]), nil
	}
}

func call2b(f func(MalType, MalType) bool) func([]MalType) (MalType, error) {
	return func(args []MalType) (MalType, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("wrong number of arguments (%d instead of 2)", len(args))
		}
		return f(args[0], args[1]), nil
	}
}
