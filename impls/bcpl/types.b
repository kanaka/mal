GET "libhdr"
GET "malhdr"

LET init_types() BE
{ nil := TABLE t_nil
  empty := TABLE t_lst, ?, ?
  empty!lst_first, empty!lst_rest := nil, empty
}

LET cons(first, rest) = VALOF
{ LET result = getvec(lst_sz)
  !result := 0
  type OF result := t_lst
  result!lst_first := first
  result!lst_rest := rest
  RESULTIS result
}

LET alloc_int(value) = VALOF
{ LET result = getvec(int_sz)
  !result := 0
  type OF result := t_int
  result!int_value := value
  RESULTIS result
}

LET str_setlen(str, len) BE
{ str!str_len := len
  (str + str_data) % 0 := len <= maxbcplstrlen -> len, maxbcplstrlen
}

LET alloc_str(len) = VALOF
{ LET result = getvec(str_data + 1 + len / bytesperword)
  !result := 0
  type OF result := t_str
  result!str_len := 0
  (result+str_data)%0 := 0
  RESULTIS result
}

LET as_sym(val) = VALOF
{ LET sym = ?
  IF type OF val = t_sym THEN RESULTIS val
  sym := alloc_str(val!str_len)
  type OF sym := t_sym
  FOR i = 1 TO str_data + val!str_len / bytesperword DO
    sym!i := val!i
  RESULTIS sym
}

LET str_bcpl2mal(bcplstr) = VALOF
{ LET result = alloc_str(bcplstr%0)
  result!str_len := bcplstr%0
  FOR i = 0 TO bcplstr%0 / bytesperword DO
    result!(str_data + i) := bcplstr!i
  RESULTIS result
}

LET alloc_vec(len) = VALOF
{ LET result = getvec(vec_data + len)
  !result := 0
  type OF result := t_vec
  result!vec_len := len
  RESULTIS result
}

LET throw(val) BE
{ last_exception := val
  longjump(catch_level, catch_label)
}
