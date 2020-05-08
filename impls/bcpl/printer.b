GET "libhdr"
GET "malhdr"

// The general structure of printing routines is:
// print_thing(..., buf, pos, count_only)
// buf is a buffer accumulating the output (only if count_only is FALSE)
// pos is the output offset within that buffer
// if count_only is FALSE, then routine will write result to buf
// in any case, pos is new output offset.

MANIFEST { pc_pos = 0; pc_buf; pc_count_only; pc_print_readably; pc_sz }

// Print a BCPL-format (constant) string.
LET print_const(pc, str) BE
{ UNLESS pc!pc_count_only DO
    FOR i = 1 TO str%0 DO
      pc!pc_buf%(pc!pc_pos + i - 1) := str%i
  pc!pc_pos := pc!pc_pos + str%0
}

// Print a single character
LET print_char(pc, chr) BE
{ UNLESS pc!pc_count_only DO pc!pc_buf%(pc!pc_pos) := chr
  pc!pc_pos := pc!pc_pos + 1
}

// Print a mal integer
LET print_int(pc, int) BE
{ LET val = int!int_value
  LET len, negative = 0, FALSE
  IF val = 0 THEN { print_char(pc, '0'); RETURN }
  IF val < 0 THEN
  { val := -val // XXX This doesnt work for the most negative integer
    len := len + 1
    negative := TRUE
  }
  WHILE val > 0 DO
  { val := val / 10
    len := len + 1
  }
  IF pc!pc_count_only THEN { pc!pc_pos := pc!pc_pos + len; RETURN }
  val := negative -> -int!int_value, int!int_value
  pc!pc_pos := pc!pc_pos + len - 1
  WHILE val > 0 DO
  { pc!pc_buf%(pc!pc_pos) :=
        (TABLE '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')!(val REM 10)
    val := val / 10
    pc!pc_pos := pc!pc_pos - 1
  }
  IF negative THEN
  { pc!pc_buf%(pc!pc_pos) := '-'
    pc!pc_pos := pc!pc_pos - 1
  }
  pc!pc_pos := pc!pc_pos + len + 1
}

// Print a mal string
LET print_str(pc, str) BE
{ print_char(pc, '*"')
  FOR i = 1 TO str!str_len DO
  { LET ch = (str + str_data)%i
    SWITCHON ch INTO
    { CASE '*n': ch := 'n'
      CASE '\': CASE '*"': print_char(pc, '\')
    }
    print_char(pc, ch)
  }
  print_char(pc, '*"')
}

LET print_sym(pc, sym) BE
{ UNLESS pc!pc_count_only DO
    FOR i = 1 TO sym!str_len DO
      pc!pc_buf%(pc!pc_pos + i - 1) := (sym + str_data)%i
  pc!pc_pos := pc!pc_pos + sym!str_len
}

LET print_kwd(pc, kwd) BE
{ print_char(pc, ':')
  print_sym(pc, kwd)
}

LET print_lst(pc, lst) BE
{ print_char(pc, '(')
  UNLESS lst = empty DO
  { print_form(pc, lst!lst_first)
    lst := lst!lst_rest
    IF lst = empty BREAK
    print_char(pc, ' ')
  } REPEAT
  print_char(pc, ')')
}

AND print_vec(pc, vec) BE
{ print_char(pc, '[')
  FOR i = vec_data TO vec_data + vec!vec_len - 1 DO
  { UNLESS i = vec_data DO print_char(pc, ' ')
    print_form(pc, vec!i)
  }
  print_char(pc, ']')
}

AND print_hmx_contents(pc, map) BE
{ print_form(pc, map!hmx_key)
  print_char(pc, ' ')
  print_form(pc, map!hmx_value)
}

AND print_hmi_contents(pc, map) BE
{ print_hm_contents(pc, map!hmi_left)
  print_char(pc, ' ')
  print_hm_contents(pc, map!hmi_right)
}

AND print_hm_contents(pc, map) BE
  TEST type OF map = t_hmi THEN print_hmi_contents(pc, map)
                           ELSE print_hmx_contents(pc, map)

AND print_hm(pc, map) BE
{ print_char(pc, '{')
  print_hm_contents(pc, map)
  print_char(pc, '}')
}

AND print_form(pc, val) BE
  SWITCHON type OF val INTO
  {
    CASE t_nil: print_const(pc, "nil"); ENDCASE
    CASE t_boo: print_const(pc, val!int_value -> "true", "false"); ENDCASE
    CASE t_lst: print_lst(pc, val); ENDCASE
    CASE t_vec: print_vec(pc, val); ENDCASE
    CASE t_hm0: print_const(pc, "{}"); ENDCASE
    CASE t_hmi:
    CASE t_hmx: print_hm (pc, val); ENDCASE
    CASE t_int: print_int(pc, val); ENDCASE
    CASE t_str: IF pc!pc_print_readably THEN { print_str(pc, val); ENDCASE }
    CASE t_sym: print_sym(pc, val); ENDCASE
    CASE t_kwd: print_kwd(pc, val); ENDCASE
    CASE t_fun:
    CASE t_mfn: print_const(pc, "#<function>"); ENDCASE
    DEFAULT: print_const(pc, "<unprintable>"); ENDCASE
  }

LET print_multi(pc, lst, space) BE
{ UNLESS lst = empty DO
  { print_form(pc, lst!lst_first)
    lst := lst!lst_rest
    IF lst = empty BREAK
    IF space THEN print_char(pc, ' ')
  } REPEAT
}

LET pr(printer, x, print_readably, A) = VALOF
{ LET pc = VEC pc_sz
  LET out = ?
  pc!pc_pos := 0
  pc!pc_count_only := TRUE
  pc!pc_print_readably := print_readably
  printer(pc, x, A)

  out := alloc_str(pc!pc_pos)
  pc!pc_buf := out + str_data
  pc!pc_pos := 1
  pc!pc_count_only := FALSE
  printer(pc, x, A)
  str_setlen(out, pc!pc_pos - 1)
  RESULTIS out
}

LET pr_str(x) = pr(print_form, x, TRUE)

LET pr_multi(x, print_readably, space) =
  pr(print_multi, x, print_readably, space)

LET print_f(pc, msg, A) BE
{ FOR i = 1 TO msg%0 DO
  { IF msg%i = '%' & i < msg%0 THEN SWITCHON msg%(i + 1) INTO
      { CASE 'v':
          print_form(pc, A)
	  i := i + 1; LOOP
      }
    print_char(pc, msg%i)
  }
}

LET throwf(msg, A) BE throw(pr(print_f, msg, A))
