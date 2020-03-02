REM > core function library for mal in BBC BASIC

REM  BBC BASIC doesn't have function pointers.  There are essentially
REM  two ways to work around this.  One is to use the BASIC EVAL function,
REM  constructing a string that will call an arbitrary function with the
REM  specified arguments.  The other is to us a big CASE statement.
REM  Following the suggestion in Hints.md, this code takes the latter
REM  approach.

DEF PROCcore_ns
  RESTORE +0
  REM  The actual DATA statements are embedded in the dispatch table below.
ENDPROC

REM  Call a core function, taking the function number and a mal list of
REM  objects to pass as arguments.
DEF FNcore_call(fn%, args%)
  LOCAL args%(), arg$
  DIM args%(1)
  CASE fn% OF
    DATA +, 0
    WHEN 0
      PROCcore_prepare_args("ii", "+")
      =FNalloc_int(args%(0) + args%(1))
    DATA -, 1
    WHEN 1
      PROCcore_prepare_args("ii", "-")
      =FNalloc_int(args%(0) - args%(1))
    DATA *, 2
    WHEN 2
      PROCcore_prepare_args("ii", "*")
      =FNalloc_int(args%(0) * args%(1))
    DATA /, 3
    WHEN 3
      PROCcore_prepare_args("ii", "/")
      =FNalloc_int(args%(0) DIV args%(1))
    DATA list, 5
    WHEN 5
      =FNas_list(args%)
    DATA list?, 6
    WHEN 6
      PROCcore_prepare_args("?", "list?")
      =FNalloc_boolean(FNis_list(args%(0)))
    DATA empty?, 7
    WHEN 7
      PROCcore_prepare_args("l", "empty?")
      =FNalloc_boolean(FNis_empty(args%(0)))
    DATA count, 8
    WHEN 8
      PROCcore_prepare_args("C", "count")
      IF FNis_nil(args%(0)) THEN =FNalloc_int(0)
      =FNalloc_int(FNcount(args%(0)))
    DATA =, 9
    WHEN 9
      PROCcore_prepare_args("??", "=")
      =FNalloc_boolean(FNcore_equal(args%(0), args%(1)))
    DATA <, 10
    WHEN 10
      PROCcore_prepare_args("ii", "<")
      =FNalloc_boolean(args%(0) < args%(1))
    DATA <=, 11
    WHEN 11
      PROCcore_prepare_args("ii", "<=")
      =FNalloc_boolean(args%(0) <= args%(1))
    DATA >, 12
    WHEN 12
      PROCcore_prepare_args("ii", ">")
      =FNalloc_boolean(args%(0) > args%(1))
    DATA >=, 13
    WHEN 13
      PROCcore_prepare_args("ii", ">=")
      =FNalloc_boolean(args%(0) >= args%(1))
    DATA read-string, 14
    WHEN 14
      PROCcore_prepare_args("t", "read-string")
      =FNread_str(args%(0))
    DATA slurp, 15
    WHEN 15
      PROCcore_prepare_args("s", "slurp")
      =FNcore_slurp(arg$)
    DATA eval, 16
    WHEN 16
      PROCcore_prepare_args("?", "eval")
      =FNEVAL(args%(0), repl_env%)
    DATA pr-str, 17
    WHEN 17
      =FNcore_print(TRUE, " ", args%)
    DATA str, 18
    WHEN 18
      =FNcore_print(FALSE, "", args%)
    DATA prn, 4
    WHEN 4
      PRINT FNunbox_string(FNcore_print(TRUE, " ", args%))
      =FNnil
    DATA println, 19
    WHEN 19
      PRINT FNunbox_string(FNcore_print(FALSE, " ", args%))
      =FNnil
    DATA atom, 20
    WHEN 20
      PROCcore_prepare_args("?", "atom")
      =FNalloc_atom(args%(0))
    DATA atom?, 21
    WHEN 21
      PROCcore_prepare_args("?", "atom?")
      =FNalloc_boolean(FNis_atom(args%(0)))
    DATA deref, 22
    WHEN 22
      PROCcore_prepare_args("a", "deref")
      =FNatom_deref(args%(0))
    DATA reset!, 23
    WHEN 23
      PROCcore_prepare_args("a?", "reset!")
      PROCatom_reset(args%(0), args%(1))
      =args%(1)
    DATA swap!, 24
    WHEN 24
      PROCcore_prepare_args("af*", "swap!")
      PROCatom_reset(args%(0), FNcore_apply(args%(1), FNalloc_pair(FNatom_deref(args%(0)), args%)))
      =FNatom_deref(args%(0))
    DATA cons, 25
    WHEN 25
      PROCcore_prepare_args("?l", "cons")
      =FNalloc_pair(args%(0), args%(1))
    DATA concat, 26
    WHEN 26
      =FNcore_concat(args%)
    DATA nth, 27
    WHEN 27
      PROCcore_prepare_args("li", "nth")
      =FNnth(args%(0), args%(1))
    DATA first, 28
    WHEN 28
      PROCcore_prepare_args("C", "first")
      IF FNis_nil(args%(0)) THEN =FNnil
      =FNfirst(args%(0))
    DATA rest, 29
    WHEN 29
      PROCcore_prepare_args("C", "rest")
      IF FNis_nil(args%(0)) THEN =FNempty
      =FNas_list(FNrest(args%(0)))
    DATA throw, 30
    WHEN 30
      PROCcore_prepare_args("?", "throw")
      MAL_ERR% = args%(0)
      ERROR &40E80900, "Mal exception: " + FNunbox_string(FNpr_str(args%(0), FALSE))
    DATA apply, 31
    WHEN 31
      PROCcore_prepare_args("f?*", "apply")
      =FNcore_apply(args%(0), FNcore_apply_args(FNalloc_pair(args%(1), args%)))
    DATA map, 32
    WHEN 32
      PROCcore_prepare_args("fl", "map")
      =FNcore_map(args%(0), args%(1))
    DATA nil?, 33
    WHEN 33
      PROCcore_prepare_args("?", "nil?")
      =FNalloc_boolean(FNis_nil(args%(0)))
    DATA true?, 34
    WHEN 34
      PROCcore_prepare_args("?", "true?")
      IF NOT FNis_boolean(args%(0)) THEN =FNalloc_boolean(FALSE)
      =args%(0)
    DATA false?, 35
    WHEN 35
      PROCcore_prepare_args("?", "false?")
      IF NOT FNis_boolean(args%(0)) THEN =FNalloc_boolean(FALSE)
      =FNalloc_boolean(NOT FNunbox_boolean(args%(0)))
    DATA symbol?, 36
    WHEN 36
      PROCcore_prepare_args("?", "symbol?")
      =FNalloc_boolean(FNis_symbol(args%(0)))
    DATA symbol, 37
    WHEN 37
      PROCcore_prepare_args("s", "symbol")
      =FNalloc_symbol(arg$)
    DATA keyword, 38
    WHEN 38
      PROCcore_prepare_args("s", "keyword")
      IF LEFT$(arg$, 1) <> CHR$(127) THEN arg$ = CHR$(127) + arg$
      =FNalloc_string(arg$)
    DATA keyword?, 39
    WHEN 39
      PROCcore_prepare_args("?", "keyword?")
      IF FNis_string(args%(0)) THEN
        =FNalloc_boolean(LEFT$(FNunbox_string(args%(0)), 1) = CHR$(127))
      ENDIF
      =FNalloc_boolean(FALSE)
    DATA vector, 40
    WHEN 40
      =FNas_vector(args%)
    DATA vector?, 41
    WHEN 41
      PROCcore_prepare_args("?", "vector?")
      =FNalloc_boolean(FNis_vector(args%(0)))      
    DATA sequential?, 42
    WHEN 42
      PROCcore_prepare_args("?", "sequential?")
      =FNalloc_boolean(FNis_seq(args%(0)))
    DATA hash-map, 43
    WHEN 43
      =FNcore_assoc(FNempty_hashmap, args%)
    DATA map?, 44
    WHEN 44
      PROCcore_prepare_args("?", "map?")
      =FNalloc_boolean(FNis_hashmap(args%(0)))
    DATA assoc, 45
    WHEN 45
      PROCcore_prepare_args("h*", "assoc")
      =FNcore_assoc(args%(0), args%)
    DATA dissoc, 46
    WHEN 46
      PROCcore_prepare_args("h*", "dissoc")
      WHILE NOT FNis_empty(args%)
        args%(0) = FNhashmap_remove(args%(0), FNunbox_string(FNfirst(args%)))
        args% = FNrest(args%)
      ENDWHILE
      =args%(0)
    DATA get, 47
    WHEN 47
      IF FNis_nil(FNfirst(args%)) THEN =FNnil
      PROCcore_prepare_args("hs", "get")
      =FNhashmap_get(args%(0), arg$)
    DATA contains?, 48
    WHEN 48
      PROCcore_prepare_args("hs", "contains?")
      =FNalloc_boolean(FNhashmap_contains(args%(0), arg$))
    DATA keys, 49
    WHEN 49
      PROCcore_prepare_args("h", "keys")
      =FNhashmap_keys(args%(0))
    DATA vals, 50
    WHEN 50
      PROCcore_prepare_args("h", "vals")
      =FNhashmap_vals(args%(0))
    DATA readline, 51
    WHEN 51
      PROCcore_prepare_args("s", "readline")
      PRINT arg$;
      LINE INPUT "" arg$
      =FNalloc_string(arg$)
    DATA meta, 52
    WHEN 52
      PROCcore_prepare_args("?", "meta")
      =FNmeta(args%(0))
    DATA with-meta, 53
    WHEN 53
      PROCcore_prepare_args("??", "with-meta")
      =FNwith_meta(args%(0), args%(1))
    DATA time-ms, 54
    WHEN 54
      PROCcore_prepare_args("", "time-ms")
      =FNalloc_int(TIME * 10)
    DATA conj, 55
    WHEN 55
      PROCcore_prepare_args("l*", "conj")
      IF FNis_list(args%(0)) THEN
        WHILE NOT FNis_empty(args%)
          args%(0) = FNalloc_pair(FNfirst(args%), args%(0))
          args% = FNrest(args%)
        ENDWHILE
        =args%(0)
      ELSE : REM args%(0) is a vector
        =FNas_vector(FNcore_concat1(args%(0), args%))
      ENDIF
    DATA string?, 56
    WHEN 56
      PROCcore_prepare_args("?", "string?")
      IF FNis_string(args%(0)) THEN
        =FNalloc_boolean(LEFT$(FNunbox_string(args%(0)), 1) <> CHR$(127))
      ENDIF
      =FNalloc_boolean(FALSE)
    DATA number?, 57
    WHEN 57
      PROCcore_prepare_args("?", "number?")
      =FNalloc_boolean(FNis_int(args%(0)))
    DATA fn?, 58
    WHEN 58
      PROCcore_prepare_args("?", "fn?")
      =FNalloc_boolean(FNis_nonmacro_fn(args%(0)) OR FNis_corefn(args%(0)))
    DATA macro?, 59
    WHEN 59
      PROCcore_prepare_args("?", "macro?")
      =FNalloc_boolean(FNis_macro(args%(0)))
    DATA seq, 60
    WHEN 60
      PROCcore_prepare_args("?", "seq")
      =FNcore_seq(args%(0))
    DATA "", -1
  ENDCASE
ERROR &40E809F1, "Call to non-existent core function"

DEF PROCcore_prepare_args(spec$, fn$)
  REM  Check that a core function is being provided with the correct
  REM  number and type of arguments and unbox them as appropriate.
  REM  spec$ is the argument specification as a string.  Each character
  REM  represents an argument:

  REM  "i" - Must be an integer;  unbox into args%()
  REM  "s" - Must be a string;    unbox into arg$
  REM  "t" - Must be a string;    stuff into args%()
  REM  "l" - Must be a sequence;  stuff into args%()
  REM  "f" - Must be a function;  stuff into args%()
  REM  "a" - Must be an atom;     stuff into args%()
  REM  "h" - Must be a hash-map;  stuff into args%()
  REM  "C" - Must be 'count'able  stuff into args%()
  REM  "?" - Any single argument  stuff into args%()
  REM  "*" - Any number of (trailing) arguments; leave in args%

  REM  This function shares some local variables with FNcore_call.

  LOCAL i%, val%

  IF RIGHT$(spec$) = "*" THEN
    spec$ = LEFT$(spec$)
    IF FNcount(args%) < LEN(spec$) THEN
      ERROR &40E80921, "Core function '"+fn$+"' requires at least "+STR$(LEN(spec$))+" arguments"
    ENDIF
  ELSE
    IF FNcount(args%) <> LEN(spec$) THEN
      ERROR &40E80921, "Core function '"+fn$+"' requires "+STR$(LEN(spec$))+" arguments"
    ENDIF
  ENDIF
  FOR i% = 1 TO LEN(spec$)
    val% = FNfirst(args%)
    CASE MID$(spec$, i%, 1) OF
      WHEN "i"
        IF NOT FNis_int(val%) THEN
          ERROR &40E80911, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be an integer"
        ENDIF
        args%(i% - 1) = FNunbox_int(val%)
      WHEN "s"
        IF NOT FNis_string(val%) THEN
          ERROR &40E80914, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be a string"
        ENDIF
        arg$ = FNunbox_string(val%)
      WHEN "t"
        IF NOT FNis_string(val%) THEN
          ERROR &40E80914, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be a string"
        ENDIF
        args%(i% - 1) = val%
      WHEN "l"
        IF NOT FNis_seq(val%) THEN
          ERROR &40E80916, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be a sequence"
        ENDIF
        args%(i% - 1) = val%
      WHEN "f"
        IF NOT FNis_fn(val%) AND NOT FNis_corefn(val%) THEN
          ERROR &40E80919, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be a function"
        ENDIF
        args%(i% - 1) = val%
      WHEN "a"
        IF NOT FNis_atom(val%) THEN
          ERROR &40E8091C, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be an atom"
        ENDIF
        args%(i% - 1) = val%
      WHEN "h"
        IF NOT FNis_hashmap(val%) THEN
          ERROR &40E8091D, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be a hash-map"
        ENDIF
        args%(i% - 1) = val%
      WHEN "C"
        IF NOT FNis_seq(val%) AND NOT FNis_nil(val%) THEN
          ERROR &40E8091F, "Argument "+STR$(i%)+" to core function '"+fn$+"' must be a countable value"
        ENDIF
        args%(i% - 1) = val%
      WHEN "?"
        args%(i% - 1) = val%
    ENDCASE
    args% = FNrest(args%)
  NEXT i%
ENDPROC

REM  Innards of the '=' function.
DEF FNcore_equal(a%, b%)
  IF a% = b% THEN =TRUE
  IF FNis_int(a%) AND FNis_int(b%) THEN =FNunbox_int(a%) = FNunbox_int(b%)
  IF FNis_symbol(a%) AND FNis_symbol(b%) THEN
    =FNunbox_symbol(a%) = FNunbox_symbol(b%)
  ENDIF
  IF FNis_string(a%) AND FNis_string(b%) THEN
    =FNunbox_string(a%) = FNunbox_string(b%)
  ENDIF
  IF FNis_seq(a%) AND FNis_seq(b%) THEN
    IF FNis_empty(a%) AND FNis_empty(b%) THEN =TRUE
    IF FNis_empty(a%) <> FNis_empty(b%) THEN =FALSE
    IF NOT FNcore_equal(FNfirst(a%), FNfirst(b%)) THEN =FALSE
    =FNcore_equal(FNrest(a%), FNrest(b%))
  ENDIF
  IF FNis_hashmap(a%) AND FNis_hashmap(b%) THEN
    REM Take advantage of the sorted keys in our hash-maps.
    IF FNcore_equal(FNhashmap_keys(a%), FNhashmap_keys(b%)) THEN
      IF FNcore_equal(FNhashmap_vals(a%), FNhashmap_vals(b%)) THEN =TRUE
    ENDIF
  ENDIF
=FALSE

REM  Innards of the 'slurp' function.
DEF FNcore_slurp(file$)
  LOCAL f%, out%
  f% = OPENIN(file$)
  IF f% = 0 THEN ERROR &40E80940, "File '"+file$+"' not found"
  out% = FNcore_slurp_channel(f%)
  CLOSE#f%
=out%

DEF FNcore_slurp_channel(f%)
  LOCAL this%
  IF EOF#f% THEN =FNalloc_string("")
  REM  GET$# doesn't include a trailing newline.
  this% = FNalloc_string(GET$#f% + CHR$(10))
=FNstring_concat(this%, FNcore_slurp_channel(f%))

REM  General-purpose printing function
DEF FNcore_print(print_readably%, sep$, args%)
  LOCAL out%
  IF FNis_empty(args%) THEN =FNalloc_string("")
  out% = FNpr_str(FNfirst(args%), print_readably%)
  args% = FNrest(args%)
  WHILE NOT FNis_empty(args%)
    out% = FNstring_append(out%, sep$)
    out% = FNstring_concat(out%, FNpr_str(FNfirst(args%), print_readably%))
    args% = FNrest(args%)
  ENDWHILE
=out%

REM  Innards of the 'apply' function, also used by 'swap!'
DEF FNcore_apply(fn%, args%)
  LOCAL ast%, env%
  IF FNis_corefn(fn%) THEN =FNcore_call(FNunbox_corefn(fn%), args%)
  IF FNis_fn(fn%) THEN
    ast% = FNfn_ast(fn%)
    env% = FNnew_env(FNfn_env(fn%), FNfn_params(fn%), args%)
    =FNEVAL(ast%, env%)
  ENDIF
ERROR &40E80918, "Not a function"

REM  Innards of 'concat' function
DEF FNcore_concat(args%)
  LOCAL tail%
  IF FNis_empty(args%) THEN =FNempty
  tail% = FNcore_concat(FNrest(args%))
=FNcore_concat1(FNfirst(args%), tail%)
  
DEF FNcore_concat1(prefix%, tail%)
  IF FNis_empty(prefix%) THEN =tail%
=FNalloc_pair(FNfirst(prefix%), FNcore_concat1(FNrest(prefix%), tail%))

REM  Recursively assemble the argument list for 'apply'
DEF FNcore_apply_args(args%)
  IF FNis_empty(FNrest(args%)) THEN =FNfirst(args%)
=FNalloc_pair(FNfirst(args%), FNcore_apply_args(FNrest(args%)))

REM  Innards of the 'map' function
DEF FNcore_map(fn%, args%)
  LOCAL car%, cdr%
  IF FNis_empty(args%) THEN =args%
  car% = FNcore_apply(fn%, FNalloc_pair(FNfirst(args%), FNempty))
  cdr% = FNcore_map(fn%, FNrest(args%))
=FNalloc_pair(car%, cdr%)

REM  Innards of the 'hash-map' function
DEF FNcore_assoc(map%, args%)
  LOCAL args%()
  DIM args%(1)
  WHILE NOT FNis_empty(args%)
    PROCcore_prepare_args("s?*", "hash-map")
    map% = FNhashmap_set(map%, arg$, args%(1))
  ENDWHILE
=map%

REM  Innards of the 'seq' function
DEF FNcore_seq(val%)
  LOCAL s$, i%
  IF FNis_empty(val%) OR FNis_nil(val%) THEN =FNnil
  IF FNis_list(val%) THEN =val%
  IF FNis_vector(val%) THEN =FNas_list(val%)
  IF FNis_string(val%) THEN
    s$ = FNunbox_string(val%)
    IF s$ = "" THEN =FNnil
    val% = FNempty
    FOR i% = LEN(s$) TO 1 STEP -1
      val% = FNalloc_pair(FNalloc_string(MID$(s$, i%, 1)), val%)
    NEXT i%
    =val%
  ENDIF
ERROR &40E8091F, "Argument to 'seq' must be list, vector, string, or nil"

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
