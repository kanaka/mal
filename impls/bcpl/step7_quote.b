GET "libhdr"
GET "malhdr"

GET "core.b"
GET "env.b"
GET "printer.b"
GET "reader.b"
GET "readline.b"
GET "types.b"

LET READ(x) = read_str(x)

// Helper function for EVAL.
LET is_sym(a, b) = VALOF
{ UNLESS type OF a = t_sym RESULTIS FALSE
  RESULTIS str_eq_const(a, b)
}

LET eval_ast(ast, env, gc_root) = VALOF
  SWITCHON type OF ast INTO
  { CASE t_sym: RESULTIS env_get(env, ast)
    CASE t_lst:
      TEST ast = empty THEN RESULTIS empty
      ELSE
      { LET first = EVAL(ast!lst_first, env, gc_root)
        LET rest = eval_ast(ast!lst_rest, env, cons(first, gc_root))
        RESULTIS cons(first, rest)
      }
    CASE t_vec:
      { LET new = alloc_vec(ast!vec_len)
        LET gc_inner_root = cons(new, gc_root)
        FOR i = 0 TO ast!vec_len - 1 DO
	  (new + vec_data)!i := EVAL((ast + vec_data)!i, env, gc_inner_root)
	RESULTIS new
      }
    CASE t_hmx:
      RESULTIS alloc_hmx(ast!hmx_key, EVAL(ast!hmx_value, env, gc_root))
    CASE t_hmi:
      { LET left  = eval_ast(ast!hmi_left,  env, gc_root)
	LET right = eval_ast(ast!hmi_right, env, cons(left, gc_root))
	RESULTIS alloc_hmi(hmi_critbit OF ast, left, right)
      }
    DEFAULT: RESULTIS ast
  }

AND EVAL(ast, env, gc_root) = VALOF
{ MANIFEST { fun_binds = fun_data; fun_body; fun_env; fun_sz }
  LET gc_inner_root = alloc_vecn(3, ast, env, gc_root)
  gc_mark(gc_inner_root)
  gc_sweep()
  UNLESS type OF ast = t_lst RESULTIS eval_ast(ast, env, gc_root)
  IF ast = empty RESULTIS ast
  { LET fn = ast!lst_first
    IF is_sym(fn, "def!") THEN
    { LET val = EVAL(nth(ast, 2), env, gc_inner_root)
      env_set(env, nth(ast, 1), val, env)
      RESULTIS val
    }
    IF is_sym(fn, "let**") THEN
    { LET newenv, bindings = env_new(env, empty, empty), as_lst(nth(ast, 1))
      UNTIL bindings = empty DO
      { env_set(newenv, bindings!lst_first,
                EVAL(nth(bindings, 1), newenv, gc_inner_root))
        bindings := bindings!lst_rest!lst_rest
      }
      ast, env := nth(ast, 2), newenv
      LOOP // TCO
    }
    IF is_sym(fn, "quote") RESULTIS ast!lst_rest!lst_first
    IF is_sym(fn, "do") THEN
    { LET tail = ast!lst_rest
      UNTIL tail!lst_rest = empty DO
      { EVAL(tail!lst_first, env, gc_inner_root)
        tail := tail!lst_rest
      }
      ast := tail!lst_first
      LOOP // TCO
    }
    IF is_sym(fn, "if") THEN
    { LET cond = EVAL(nth(ast, 1), env, gc_inner_root)
      LET tail = ast!lst_rest!lst_rest
      IF cond = nil | cond = mfalse THEN tail := tail!lst_rest
      ast := tail!lst_first
      LOOP // TCO
    }
    IF is_sym(fn, "fn**") THEN
    { LET call(fun, args, gc_root) =
          EVAL(fun!fun_body, env_new(fun!fun_env, fun!fun_binds, args), gc_root)
      LET result = alloc_fun(call, fun_sz,
                             as_lst(nth(ast, 1)), nth(ast, 2), env)
      fun_ntracked OF result := 3
      type OF result := t_mfn
      RESULTIS result
    }
  }
  ast := eval_ast(ast, env, gc_inner_root)
  { LET fn, args = ast!lst_first, ast!lst_rest
    UNLESS supertype OF fn = t_fun DO throwf("not a function")
    IF type OF fn = t_mfn THEN
    { ast, env := fn!fun_body, env_new(fn!fun_env, fn!fun_binds, args)
      LOOP // TCO
    }
    RESULTIS (fn!fun_code)(fn, args, gc_root)
  }
} REPEAT

LET PRINT(x) = pr_str(x)

STATIC { repl_env }

LET rep(x) = PRINT(EVAL(READ(x), repl_env), nil)

LET repl(argv) BE
{ LET mal_eval(fn, args, gc_root) = EVAL(args!lst_first, repl_env, gc_root)
  repl_env := core_env()
  env_set(repl_env, as_sym(str_bcpl2mal("eval")), alloc_fun(mal_eval, fun_data))
  env_set(repl_env, as_sym(str_bcpl2mal("**FILE**")), argv!lst_first)
  env_set(repl_env, as_sym(str_bcpl2mal("**ARGV**")), argv!lst_rest)
  rep(str_bcpl2mal("(def! not (fn** (a) (if a false true)))"), repl_env)
  rep(str_bcpl2mal("(def! load-file (fn** (f) (eval (read-string *
                    *(str *"(do *" (slurp f) *"*nnil)*")))))"), repl_env)
  UNLESS argv = empty DO
  {
    catch_level, catch_label := level(), uncaught_exit
    rep(str_bcpl2mal("(load-file **FILE**)"))
    sys(Sys_quit, 0)
    uncaught_exit:
    writes("Uncaught exception: ")
    writes(@(pr_str(last_exception)!str_data))
    newline()
    sys(Sys_quit, 1)
  }
  catch_level, catch_label := level(), uncaught
  IF FALSE THEN
  { uncaught:
    writes("Uncaught exception: ")
    writes(@(pr_str(last_exception)!str_data))
    newline()
  }
  { LET prompt = str_bcpl2mal("user> ")
    LET line = readline(prompt)
    IF line = nil THEN BREAK
    writes(@rep(line)!str_data)
    newline()
    gc_mark(repl_env)
    gc_sweep()
  } REPEAT
}

// This is a cut-down version of the reader's tokenize function.

// Cintsys passes us the entire command line as a single string and doesn't
// quote values in any way, so we can't reliably reconstruct arguments with
// whitespace in them.
LET read_argv() = VALOF
{ LET s = readline(str_bcpl2mal(""))
  LET tokens, tail = empty, empty
  LET sd = s + str_data
  LET tokstart, token = ?, ?
  FOR p = 1 TO s!str_len DO
  { tokstart := p
    // Within this SWITCHON command, use LOOP to ignore input, or ENDCASE to
    // emit a token.
    SWITCHON sd%p INTO
    { CASE ' ': CASE '*t': CASE '*n': LOOP // Inter-token whitespace
      DEFAULT: // Word
        WHILE p < s!str_len DO
        { p := p + 1
	  SWITCHON sd%p INTO
          { CASE ' ': CASE '*t': CASE '*n':
              p := p - 1; BREAK
          }
        }
        ENDCASE
    }
    // At this point, tokstart points to the first character of the token,
    // and p points to the last character.
    token := str_substr(s, tokstart, p + 1)
    TEST tokens = empty THEN
    { tokens := cons(token, empty)
      tail := tokens
    } ELSE
    { tail!lst_rest := cons(token, empty)
      tail := tail!lst_rest
    }
  }
  RESULTIS tokens
}

LET start() = VALOF
{ init_types()
  wrch('*n') // Terminate prompt printed by Cintsys
  repl(read_argv())
  RESULTIS 0
}
