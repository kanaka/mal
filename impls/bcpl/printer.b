GET "libhdr"
GET "malhdr"

// The general structure of printing routines is:
// print_thing(..., buf, pos, count_only)
// buf is a buffer accumulating the output (only if count_only is FALSE)
// pos is the output offset within that buffer
// if count_only is FALSE, then routine will write result to buf
// in any case, pos is new output offset.

// Print a BCPL-format (constant) string.
LET print_const(str, buf, pos, count_only) = VALOF
{ UNLESS count_only DO
    FOR i = 1 TO str%0 DO
      buf%(pos + i - 1) := str%i
  RESULTIS pos + str%0
}

// Print a single character
LET print_char(chr, buf, pos, count_only) = VALOF
{ UNLESS count_only DO buf%pos := chr
  RESULTIS pos + 1
}

// Print a mal integer
LET print_int(int, buf, pos, count_only) = VALOF
{ LET val = int!int_value
  LET len, negative = 0, FALSE
  IF val = 0 RESULTIS print_char('0', buf, pos, count_only)
  IF val < 0 THEN
  { val := -val // XXX This doesnt work for the most negative integer
    len := len + 1
    negative := TRUE
  }
  WHILE val > 0 DO
  { val := val / 10
    len := len + 1
  }
  IF count_only RESULTIS pos + len
  val := negative -> -int!int_value, int!int_value
  pos := pos + len - 1
  WHILE val > 0 DO
  { buf%pos :=
        (TABLE '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')!(val REM 10)
    val := val / 10
    pos := pos - 1
  }
  IF negative THEN
  { buf%pos := '-'
    pos := pos - 1
  }
  RESULTIS pos + len + 1
}

// Print a mal string
LET print_str(str, buf, pos, count_only) = VALOF
{ pos := print_char('*"', buf, pos, count_only)
  FOR i = 1 TO str!str_len DO
  { LET ch = (str + str_data)%i
    SWITCHON ch INTO
    { CASE '*n': ch := 'n'
      CASE '\': CASE '*"': pos := print_char('\', buf, pos, count_only)
    }
    pos := print_char(ch, buf, pos, count_only)
  }
  RESULTIS print_char('*"', buf, pos, count_only)
}

LET print_sym(sym, buf, pos, count_only) = VALOF
{ UNLESS count_only DO
    FOR i = 1 TO sym!str_len DO
      buf%(pos + i - 1) := (sym + str_data)%i
  RESULTIS pos + sym!str_len
}

LET print_lst(lst, buf, pos, count_only) = VALOF
{ pos := print_char('(', buf, pos, count_only)
  UNLESS lst = empty DO
  { pos := print_form(lst!lst_first, buf, pos, count_only)
    lst := lst!lst_rest
    IF lst = empty BREAK
    pos := print_char(' ', buf, pos, count_only)
  } REPEAT
  RESULTIS print_char(')', buf, pos, count_only)
}

AND print_vec(vec, buf, pos, count_only) = VALOF
{ pos := print_char('[', buf, pos, count_only)
  FOR i = vec_data TO vec_data + vec!vec_len - 1 DO
  { UNLESS i = vec_data DO pos := print_char(' ', buf, pos, count_only)
    pos := print_form(vec!i, buf, pos, count_only)
  }
  RESULTIS print_char(']', buf, pos, count_only)
}

AND print_form(val, buf, pos, count_only) = VALOF
  SWITCHON type OF val INTO
  {
    CASE t_nil: RESULTIS print_const("nil", buf, pos, count_only)
    CASE t_lst: RESULTIS print_lst(val, buf, pos, count_only)
    CASE t_vec: RESULTIS print_vec(val, buf, pos, count_only)
    CASE t_int: RESULTIS print_int(val, buf, pos, count_only)
    CASE t_str: RESULTIS print_str(val, buf, pos, count_only)
    CASE t_sym: RESULTIS print_sym(val, buf, pos, count_only)
    DEFAULT: RESULTIS print_const("<unprintable>", buf, pos, count_only)
  }

LET pr_str(x) = VALOF
{ LET count = print_form(x, 0, 0, TRUE)
  LET out = alloc_str(count)
  print_form(x, out + str_data, 1, FALSE)
  str_setlen(out, count)
  RESULTIS out
}
