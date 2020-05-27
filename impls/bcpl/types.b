GET "libhdr"
GET "malhdr"

LET init_types() BE
{ // These objects are statically-allocated and hence special.
  // nil acts as the head and tail of the global object list.
  nil := TABLE ?, t_nil
  nil!nextptr := nil
  empty := TABLE ?, t_lst, ?, ?
  empty!lst_first, empty!lst_rest := nil, empty
  empty_hashmap := TABLE ?, t_hm0
  mtrue  := TABLE ?, t_boo, TRUE
  mfalse := TABLE ?, t_boo, FALSE
}

STATIC { new_objects = 0; old_objects = 0 }

LET alloc_val(size) = VALOF
{ LET result = getvec(size)
  result!1 := 0 // Make sure type word is all zeroes.
  result!nextptr := nil!nextptr
  nil!nextptr := result
  new_objects := new_objects + 1
  // writef("ALLOC: <- %8x*n", result)
  RESULTIS result
}

LET gc_mark(x) BE
{ IF gc_marked OF x = 1 THEN RETURN
  // writef("MARK : -- %8x (%8x)*n", x, x!1)
  gc_marked OF x := 1
  // Note manual tail-call elimination here so that marking a long
  // list doesn't cause a stack overflow.  Other large data structures
  // still could, though.
  SWITCHON supertype OF x INTO
  { CASE t_lst: gc_mark(x!lst_first); x := x!lst_rest; LOOP
    CASE t_vec: FOR i = 0 TO x!vec_len - 1 DO
                  gc_mark((x+vec_data)!i)
		ENDCASE
    CASE t_hmi: gc_mark(x!hmi_left); x := x!hmi_right; LOOP
    CASE t_env: gc_mark(x!env_data); x := x!env_outer; LOOP
    CASE t_fun: FOR i = 0 TO (fun_ntracked OF x) - 1 DO
                  gc_mark((x+fun_data)!i)
		ENDCASE
    CASE t_atm: x := x!atm_value; LOOP
  }
  RETURN
} REPEAT

LET gc_sweep() BE
{ LET last, this = nil, nil!nextptr
  old_objects := 0
  UNTIL this = nil DO
  { TEST gc_marked OF this THEN
    { gc_marked OF this := 0
      old_objects := old_objects + 1
      last, this := this, this!nextptr
    } ELSE
    { LET tmp = this
      this := this!nextptr
      // writef("FREE : -> %8x (%8x)*n", tmp, tmp!1)
      freevec(tmp)
      last!nextptr := this
    }
  }
}

LET gc(x) BE
{ IF new_objects > old_objects THEN
  { // writef("Starting GC: ctr = %0d; last = %0d*n", alloc_ctr, alloc_last)
    gc_mark(x)
    gc_sweep()
    new_objects := 0
    // writef("GC done: last = %0d*n", alloc_last)
  }
}

LET cons(first, rest) = VALOF
{ LET result = alloc_val(lst_sz)
  type OF result := t_lst
  result!lst_first := first
  result!lst_rest := rest
  RESULTIS result
}

LET nth(lst, n) = VALOF
{ UNTIL n = 0 DO lst, n := lst!lst_rest, n - 1
  IF lst = empty THEN throwf("subscript out of range")
  RESULTIS lst!lst_first
}

LET as_lst(x) = VALOF SWITCHON type OF x INTO
{ CASE t_lst: RESULTIS x
  CASE t_nil: RESULTIS empty
  CASE t_vec:
    { LET l = empty
      FOR i = x!vec_len - 1 TO 0 BY -1 DO
        l := cons((x+vec_data)!i, l)
      RESULTIS l
    }
  DEFAULT:
    throwf("cannot convert to list")
}
  

LET alloc_int(value) = VALOF
{ LET result = alloc_val(int_sz)
  type OF result := t_int
  result!int_value := value
  RESULTIS result
}

LET str_setlen(str, len) BE
{ str!str_len := len
  (str + str_data) % 0 := len <= maxbcplstrlen -> len, maxbcplstrlen
}

LET alloc_str(len) = VALOF
{ LET words = str_data + 1 + len / bytesperword
  LET result = alloc_val(words)
  result!(words - 1) := 0 // Make sure the unused part word at the end is 0.
  type OF result := t_str
  result!str_len := 0
  (result+str_data)%0 := 0
  RESULTIS result
}

LET str_dup(val) = VALOF
{ LET new = alloc_str(val!str_len)
  FOR i = 1 TO str_data + val!str_len / bytesperword DO
    new!i := val!i
  RESULTIS new
}

LET str_substr(s, start, end) = VALOF
{ LET len = end - start
  LET ss = alloc_str(len)
  FOR i = 1 TO len DO
    (ss + str_data) % i := (s + str_data) % (start + i - 1)
  str_setlen(ss, len)
  RESULTIS ss
}

LET as_strtype(val, want_type) = VALOF
{ IF type OF val = want_type THEN RESULTIS val
  val := str_dup(val)
  type OF val := want_type
  RESULTIS val
}

LET as_str(val) = as_strtype(val, t_str)
LET as_sym(val) = as_strtype(val, t_sym)
LET as_kwd(val) = as_strtype(val, t_kwd)

LET str_bcpl2mal(bcplstr) = VALOF
{ LET result = alloc_str(bcplstr%0)
  result!str_len := bcplstr%0
  FOR i = 0 TO bcplstr%0 / bytesperword DO
    result!(str_data + i) := bcplstr!i
  RESULTIS result
}

LET str_eq_const(val, bcplstr) = VALOF
{ UNLESS val!str_len = bcplstr%0 RESULTIS FALSE
  FOR i = 0 TO bcplstr%0 / bytesperword DO
    UNLESS val!(str_data + i) = bcplstr!i RESULTIS FALSE
  RESULTIS TRUE
}

LET equal_scalar(a, b) = VALOF
{ LET len = ?
  UNLESS type OF a = type OF b RESULTIS FALSE
  len := VALOF SWITCHON supertype OF a INTO
  { CASE t_nil: RESULTIS 1
    CASE t_int: RESULTIS int_sz
    CASE t_str: RESULTIS str_data + 1 + a!str_len / bytesperword
    DEFAULT: throwf("incomparable value: %v", a)
  }
  // This is guaranteed not to walk off the end of b because any two mal
  // values with different lengths will differ before the point where
  // either of them ends.
  FOR i = 1 TO len - 1 DO
    UNLESS a!i = b!i RESULTIS FALSE
  RESULTIS TRUE
}

LET equal(a, b) = VALOF
{ UNLESS type OF a = type OF b RESULTIS equal_mixed(a, b)
  SWITCHON type OF a INTO
  { CASE t_lst: RESULTIS equal_lst(a, b)
    CASE t_vec: RESULTIS equal_vec(a, b)
    DEFAULT:    RESULTIS equal_scalar(a, b)
  }
}

AND equal_mixed(a, b) = VALOF
{ // Mostly, values of different types are unequal.  However mal has a
  // special rule that a vector and a list are equal if they have the same
  // contents.
  IF type OF a = t_lst & type OF b = t_vec RESULTIS equal_lstvec(a, b)
  IF type OF a = t_vec & type OF b = t_lst RESULTIS equal_lstvec(b, a)
  RESULTIS FALSE
}

AND equal_lst(a, b) = VALOF
{ IF a = b = empty RESULTIS TRUE
  IF a = empty | b = empty RESULTIS FALSE
  UNLESS equal(a!lst_first, b!lst_first) RESULTIS FALSE
  a, b := a!lst_rest, b!lst_rest
} REPEAT

AND equal_vec(a, b) = VALOF
{ UNLESS a!vec_len = b!vec_len RESULTIS FALSE
  FOR i = 0 TO a!vec_len - 1 DO
    UNLESS equal((a + vec_data)!i, (b + vec_data)!i) RESULTIS FALSE
  RESULTIS TRUE
}

AND equal_lstvec(l, v) = VALOF
{ FOR i = 0 TO v!vec_len - 1 DO
  { IF l = empty RESULTIS FALSE
    UNLESS equal(l!lst_first, (v + vec_data)!i) RESULTIS FALSE
    l := l!lst_rest
  }
  RESULTIS l = empty
}

LET alloc_vec(len) = VALOF
{ LET result = alloc_val(vec_data + len)
  type OF result := t_vec
  result!vec_len := len
  FOR i = 0 TO len - 1 DO
    (result + vec_data)!i := nil
  RESULTIS result
}

LET alloc_vecn(n, A, B, C, D) = VALOF
{ LET result = alloc_vec(n)
  FOR i = 0 TO n - 1 DO
    (result + vec_data)!i := (@A)!i
  RESULTIS result
}

LET alloc_fun(fn, sz, A, B, C) = VALOF
{ LET result = alloc_val(sz)
  LET p = @A
  type OF result := t_fun
  result!fun_code := fn
  FOR i = 0 TO sz - fun_data - 1
    result!(fun_data + i) := p!i
  RESULTIS result
}

LET alloc_hmx(key, value) = VALOF
{ LET result = alloc_val(hmx_sz)
  type OF result := t_hmx
  result!hmx_key, result!hmx_value := key, value
  RESULTIS result
}

LET alloc_hmi(critbit, left, right) = VALOF
{ LET result = alloc_val(hmi_sz)
  type OF result := t_hmi
  hmi_critbit OF result := critbit
  result!hmi_left, result!hmi_right := left, right
  RESULTIS result
}

LET key_bit(key, bit) = VALOF
{ LET offset, shift = bit / BITSPERBCPLWORD, bit REM BITSPERBCPLWORD
  // Skip over the first word because it's not part of the value.
  offset := offset + 1
  RESULTIS key!offset >> (BITSPERBCPLWORD - 1 - shift) & 1
}

LET key_bitdiff(key1, key2) = VALOF
{ LET bit = 0
  WHILE key_bit(key1, bit) = key_bit(key2, bit) DO bit := bit + 1
  RESULTIS bit
}

LET hm_pfx(pfx) BE
  WHILE pfx > 0 DO
  { writes(" ")
    pfx := pfx - 1
  }

LET hm_dumpi(map, pfx, lastbit) BE
{ hm_pfx(pfx)
  TEST type OF map = t_hmi THEN
  { writef("%8x: bit %n*n", map, hmi_critbit OF map)
    hm_dumpi(map!hmi_left, pfx + 1,  hmi_critbit OF map)
    hm_dumpi(map!hmi_right, pfx + 1,  hmi_critbit OF map)
  }
  ELSE
  { LET p = map!hmx_key + 1
    writef("%8x:", map)
    WHILE lastbit > 0 DO
    { writef(" %8x", !p)
      p := p + 1
      lastbit := lastbit - BITSPERBCPLWORD
    }
    writef(" v: %8x*n", map!hmx_value)
  }
}

LET hm_dump(map) BE
{ TEST map = empty_hashmap THEN writes("[empty]*n")
                           ELSE hm_dumpi(map, 0, 0)
}

// hm_find finds the nearest entry in a non-empty hash-map to
// the key requested, and returns the entire entry.
LET hm_find(map, key) = VALOF
{ WHILE type OF map = t_hmi DO
    map := key_bit(key, hmi_critbit OF map) -> map!hmi_right, map!hmi_left
  RESULTIS map
}

// Replace a known-present key in a non-empty hashmap.
LET hm_replace(map, key, value) = VALOF
{ LET left, right = ?, ?
  IF type OF map = t_hmx RESULTIS alloc_hmx(key, value)
  left, right := map!hmi_left, map!hmi_right
  TEST key_bit(key, hmi_critbit OF map) THEN
    right := hm_replace(map!hmi_right, key, value)
  ELSE
    left := hm_replace(map!hmi_left, key, value)
  RESULTIS alloc_hmi(hmi_critbit OF map, left, right)
}

// Add a known-absent key into a non-empty hashmap.  It's known that the
// first bit where it differs from any existing key is 'bit'.
LET hm_insert(map, bit, key, value) = VALOF
{ LET left, right = ?, ?
  IF type OF map = t_hmi & hmi_critbit OF map < bit THEN
  { left, right := map!hmi_left, map!hmi_right
    TEST key_bit(key,  hmi_critbit OF map) THEN
      right := hm_insert(map!hmi_right, bit, key, value)
    ELSE
      left  := hm_insert(map!hmi_left, bit, key, value)
    RESULTIS alloc_hmi(hmi_critbit OF map, left, right)
  }
  TEST key_bit(key, bit) THEN left, right := map, alloc_hmx(key, value)
                         ELSE right, left := map, alloc_hmx(key, value)
  RESULTIS alloc_hmi(bit, left, right)
}

LET hm_set(map, key, value) = VALOF
{ LET bit, nearest = ?, ?
  IF compoundflag OF key = 1 THEN throwf("invalid hash-map key: %v", key)
  IF map = empty_hashmap RESULTIS alloc_hmx(key, value)
  nearest := hm_find(map, key)
  IF equal(nearest!hmx_key, key) THEN RESULTIS hm_replace(map, key, value)
  bit := key_bitdiff(key, nearest!hmx_key)
  RESULTIS hm_insert(map, bit, key, value)
}

LET hm_remove(map, key) = VALOF
{ IF compoundflag OF key = 1 THEN throwf("invalid hash-map key: %v", key)
  IF map = empty_hashmap RESULTIS map
  IF type OF map = t_hmx THEN
    RESULTIS equal(map!hmx_key, key) -> empty_hashmap, map
  TEST key_bit(key, hmi_critbit OF map) THEN
  { LET child = hm_remove(map!hmi_right, key)
    IF child = empty_hashmap RESULTIS map!hmi_left
    RESULTIS alloc_hmi(hmi_critbit OF map, map!hmi_left, child)
  } ELSE
  { LET child = hm_remove(map!hmi_left, key)
    IF child = empty_hashmap RESULTIS map!hmi_right
    RESULTIS alloc_hmi(hmi_critbit OF map, child, hmi_right)
  }
}

LET hm_get(map, key) = VALOF
{ IF compoundflag OF key = 1 THEN throwf("invalid hash-map key: %v", key)
  IF map = empty_hashmap RESULTIS nil
  map := hm_find(map, key)
  RESULTIS equal(map!hmx_key, key) -> map!hmx_value, nil
}

LET hm_contains(map, key) = VALOF
{ IF compoundflag OF key = 1 THEN throwf("invalid hash-map key: %v", key)
  IF map = empty_hashmap RESULTIS FALSE
  map := hm_find(map, key)
  RESULTIS equal(map!hmx_key, key)
}

LET alloc_atm(value) = VALOF
{ LET result = alloc_val(atm_sz)
  type OF result := t_atm
  result!atm_value := value
  RESULTIS result
}

LET throw(val) BE
{ last_exception := val
  longjump(catch_level, catch_label)
}
