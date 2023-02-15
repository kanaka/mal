module mal

import maps

type Type = Atom
	| Closure
	| False
	| Float
	| Fn
	| Hashmap
	| Int
	| Keyword
	| List
	| Nil
	| String
	| Symbol
	| True
	| Vector

type FnDef = fn (args List) !Type

// convert one or both arguments, where such an upward, non-destructive
// conversion would enable their comparison
fn implicit_conv(a_ Type, b_ Type) !(Type, Type) {
	a := a_.resolve_atom()
	b := b_.resolve_atom()
	// same type
	if a.type_idx() == b.type_idx() {
		return a, b
	}
	// automatic conversion
	if a is Int && b is Float {
		return Float{a.val}, b
	}
	if a is Float && b is Int {
		return a, Float{b.val}
	}
	if a is Vector && b is List {
		return new_list(a.vec), b
	}
	if a is List && b is Vector {
		return a, new_list(b.vec)
	}
	// fail
	return error('type mismatch')
}

pub fn make_bool(cond bool) Type {
	return if cond { True{} } else { False{} }
}

pub fn (t Type) truthy() bool {
	return !t.falsey()
}

pub fn (t Type) falsey() bool {
	return t in [False, Nil]
}

pub fn (t Type) call_sym() ?string {
	if t is List {
		if t.list.len > 0 {
			list0 := t.list[0]
			if list0 is Symbol {
				return list0.sym
			}
		}
	}
	return none
}

pub fn (t Type) key() !string {
	return match t {
		String { '"${t.val}' }
		Keyword { ':${t.kw}' }
		else { error('bad key') }
	}
}

pub fn unkey(key string) Type {
	return match key[0] {
		`:` { Keyword{key[1..]} }
		`"` { String{key[1..]} }
		else { Nil{} }
	}
}

pub fn (t Type) numeric() bool {
	return t in [Int, Float]
}

pub fn (t Type) resolve_atom() Type {
	return if t is Atom { t.typ.resolve_atom() } else { t }
}

pub fn (t Type) eq(o Type) bool {
	a, b := implicit_conv(t, o) or { return false }
	match a {
		List {
			if a.list.len != (b as List).list.len {
				return false
			}
			for i, aa in a.list {
				if !aa.eq((b as List).list[i]) {
					return false
				}
			}
			return true
		}
		Vector {
			if a.vec.len != (b as Vector).vec.len {
				return false
			}
			for i, aa in a.vec {
				if !aa.eq((b as Vector).vec[i]) {
					return false
				}
			}
			return true
		}
		Int {
			return a.val == (b as Int).val
		}
		Float {
			return a.val == (b as Float).val
		}
		String {
			return a.val == (b as String).val
		}
		True {
			return b is True
		}
		False {
			return b is False
		}
		Nil {
			return b is Nil
		}
		Keyword {
			return a.kw == (b as Keyword).kw
		}
		Symbol {
			return a.sym == (b as Symbol).sym
		}
		Hashmap {
			if a.hm.len != (b as Hashmap).hm.len {
				return false
			}
			for k, v in a.hm {
				bv := (b as Hashmap).hm[k] or { return false }
				if !v.eq(bv) {
					return false
				}
			}
			return true
		}
		Fn {
			return a.f == (b as Fn).f
		}
		Closure {
			bc := b as Closure
			return a.env == bc.env && a.ast == bc.ast && a.params == bc.params
		}
		Atom {
			panic('unresolved atom')
		}
	}
}

pub fn (t Type) lt(o Type) !bool {
	a, b := implicit_conv(t, o)!
	return match a {
		Int { a.val < (b as Int).val }
		Float { a.val < (b as Float).val }
		String { a.val < (b as String).val }
		else { error('invalid comparison') }
	}
}

pub fn (t Type) get_meta() !Type {
	return match t {
		Fn, Closure, List, Vector, Hashmap {
			t.meta
		}
		else {
			error('metadata not supported here')
		}
	}
}

pub fn (t Type) set_meta(meta Type) !Type {
	return match t {
		// https://github.com/vlang/v/issues/17333
		// Fn, Closure, List, Vector, Hashmap {
		Fn {
			t.with_meta(meta)
		}
		Closure {
			t.with_meta(meta)
		}
		List {
			t.with_meta(meta)
		}
		Vector {
			t.with_meta(meta)
		}
		Hashmap {
			t.with_meta(meta)
		}
		else {
			error('metadata not supported here')
		}
	}
}

pub fn (t Type) sym() !string {
	return if t is Symbol { t.sym } else { error('symbol expected') }
}

pub fn (t Type) fn_() !FnDef {
	return if t is Fn { t.f } else { error('function expected') }
}

pub fn (t Type) cls() !&Closure {
	return if t is Closure { &t } else { error('closure expected') }
}

pub fn (t Type) int_() !i64 {
	return if t is Int { t.val } else { error('integer expected') }
}

pub fn (t Type) str_() !string {
	return if t is String { t.val } else { error('string expected') }
}

pub fn (t Type) list() ![]Type {
	return if t is List { t.list } else { error('list expected') }
}

pub fn (t Type) sequence() ![]Type {
	return match t {
		List { t.list }
		Vector { t.vec }
		Nil { []Type{} }
		else { error('list/vector expected') }
	}
}

pub fn (t &Type) atom() !&Atom {
	return if t is Atom { unsafe { &t } } else { error('atom expected') }
}

pub fn (t &Type) hashmap() !Hashmap {
	return match t {
		Hashmap { unsafe { t } }
		Nil { new_hashmap({}) }
		else { error('hashmap expected') }
	}
}

// --

pub struct Int {
pub:
	val i64
}

// --

pub struct Float {
pub:
	val f64
}

// --

pub struct String {
pub:
	val string
}

// -

pub struct Keyword {
pub:
	kw string
}

// --

pub struct Nil {}

// --

pub struct True {}

// --

pub struct False {}

// --

pub struct Symbol {
pub:
	sym string
}

// --

pub struct List {
pub:
	list []Type
	meta Type = Nil{}
}

pub fn new_list(list []Type) List {
	return List{
		list: list
	}
}

pub fn (l &List) with_meta(meta Type) Type {
	return Type(List{
		list: l.list
		meta: meta
	})
}

pub fn (l &List) first() !&Type {
	return if l.list.len > 0 { &l.list[0] } else { error('list: empty') }
}

pub fn (l &List) last() !&Type {
	return if l.list.len > 0 { &l.list[l.list.len - 1] } else { error('list: empty') }
}

pub fn (l &List) rest() List {
	return l.from(1)
}

pub fn (l &List) from(n int) List {
	return if l.list.len > n { new_list(l.list[n..]) } else { List{} }
}

pub fn (l List) len() int {
	return l.list.len
}

pub fn (l &List) nth(n int) &Type {
	return if n < l.list.len { &l.list[n] } else { Nil{} }
}

pub fn (l &List) conj(list List) Type {
	mut list_ := l.list[0..]
	list_.prepend(list.list.reverse())
	return Type(List{
		...l
		list: list_
	})
}

// --

pub struct Vector {
pub:
	vec  []Type
	meta Type = Nil{}
}

pub fn new_vector(vec []Type) Vector {
	return Vector{
		vec: vec
	}
}

pub fn (v &Vector) with_meta(meta Type) Type {
	return Vector{
		vec: v.vec
		meta: meta
	}
}

pub fn (v &Vector) conj(list List) Type {
	mut vec := v.vec[0..]
	vec.insert(v.vec.len, list.list)
	return Vector{
		...v
		vec: vec
	}
}

// --

pub struct Hashmap {
pub:
	hm   map[string]Type
	meta Type = Nil{}
}

pub fn new_hashmap(from map[string]Type) Hashmap {
	return Hashmap{
		hm: from
	}
}

fn (m map[string]Type) copy() map[string]Type {
	mut hm := map[string]Type{}
	for k, v in m {
		hm[k] = v
	}
	return hm
}

pub fn (h &Hashmap) load(list List) !Hashmap {
	mut hm := h.hm.copy()
	mut list_ := list.list[0..] // copy
	if list_.len % 2 == 1 {
		return error('extra param')
	}
	for list_.len > 0 {
		k, v := list_[0], list_[1]
		hm[k.key()!] = v
		list_ = list_[2..]
	}
	return Hashmap{
		hm: hm
		meta: h.meta
	}
}

pub fn (h &Hashmap) with_meta(meta Type) Type {
	return Hashmap{
		hm: h.hm
		meta: meta
	}
}

pub fn (h &Hashmap) filter(list List) !Hashmap {
	mut list_ := list.list.map(it.key()!)
	return new_hashmap(maps.filter(h.hm, fn [list_] (k string, _ Type) bool {
		return k !in list_
	}))
}

pub fn (h &Hashmap) get(key string) Type {
	if val := h.hm[key] {
		return val
	} else {
		return Nil{}
	}
}

pub fn (h &Hashmap) has(key string) bool {
	return if _ := h.hm[key] { true } else { false }
}

// --

[heap]
pub struct Fn {
pub:
	f    FnDef
	meta Type = Nil{}
}

pub fn new_fn(f FnDef) Fn {
	return Fn{
		f: f
	}
}

pub fn (f &Fn) with_meta(meta Type) Type {
	ff := Fn{
		...f
		meta: meta
	}
	return ff
}

fn (f &Fn) str() string {
	meta := f.meta.str().replace('\n', '\n    ')
	return 'mal.Fn{\n    <fn>\n    meta: ${meta}\n}'
}

// --

pub struct Closure {
pub:
	ast      Type
	params   []string
	env      &Env
	is_macro bool
	meta     Type = Nil{}
}

pub fn new_closure(ast Type, params []string, env &Env) Closure {
	return Closure{
		ast: ast
		params: params
		env: env
	}
}

pub fn (c &Closure) with_meta(meta Type) Type {
	return Closure{
		...c
		meta: meta
	}
}

fn (c Closure) str() string {
	disp := if c.is_macro { 'macro' } else { 'closure' }
	meta := c.meta.str().replace('\n', '\n    ')
	return 'mal.Closure{\n    <${disp}>\n    meta: ${meta}\n}'
}

pub fn (c Closure) to_macro() Closure {
	return Closure{
		...c
		is_macro: true
	}
}

// --

pub struct Atom {
pub mut:
	typ Type = Nil{}
}

fn (a &Atom) set(t Type) Type {
	mut mut_a := unsafe { a }
	mut_a.typ = t
	return t
}

// --

struct Exception {
	Error
pub:
	typ Type
}

fn (e Exception) msg() string {
	return 'Exception'
}
