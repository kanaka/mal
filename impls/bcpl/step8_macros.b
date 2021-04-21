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

LET is_pair(ast) =
  type OF ast = t_lst & ast ~= empty | type OF ast = t_vec & ast!vec_len > 0

LET quasiquote(ast) = VALOF
{ UNLESS is_pair(ast)
    RESULTIS cons(as_sym(str_bcpl2mal("quote")), cons(ast, empty))
  ast := as_lst(ast)
  IF is_sym(ast!lst_first, "unquote") RESULTIS ast!lst_rest!lst_first
  IF is_pair(ast!lst_first) & is_sym(ast!lst_first!lst_first, "splice-unquote")
    RESULTIS cons(as_sym(str_bcpl2mal("concat")),
                  cons(ast!lst_first!lst_rest!lst_first,
		       cons(quasiquote(ast!lst_rest), empty)))
  RESULTIS cons(as_sym(str_bcpl2mal("cons")),
                cons(quasiquote(ast!lst_first),
		     cons(quasiquote(ast!lst_rest), empty)))
}

LET is_macro_call(ast, env) = VALOF
{ LET fn = ?
  UNLESS type OF ast = t_lst & type OF (ast!lst_first) = t_sym &
         env_find(env, ast!lst_first) ~= nil RESULTIS FALSE
  fn := env_get(env, ast!lst_first)
  RESULTIS type OF fn = t_mfn & mfn_ismacro OF fn ~= 0
}

LET macroexpand(ast, env, gc_root) = VALOF
{ WHILE is_macro_call(ast, env) DO
  { LET fn = env_get(env, ast!lst_first)
    ast := (fn!fun_code)(fn, ast!lst_rest, gc_root)
  }
  RESULTIS ast
}

LET eval_ast(ast, env, gc_root) = VALOF
  SWITCHON type OF ast INTO
  { CASE t_sym: RESULTIS env_get(env, ast)
    CASE t_lst:
      TEST ast = empty THEN RESULTIS empty
      ELSE
      { LET gc_inner_root = alloc_vecn(3, ast, env, gc_root)
        LET first = EVAL(ast!lst_first, env, gc_inner_root)
        LET rest = eval_ast(ast!lst_rest, env, cons(first, gc_root))
        RESULTIS cons(first, rest)
      }
    CASE t_vec:
      { LET new = alloc_vec(ast!vec_len)
        LET gc_inner_root = alloc_vecn(4, new, ast, env, gc_root)
        FOR i = 0 TO ast!vec_len - 1 DO
	  (new + vec_data)!i := EVAL((ast + vec_data)!i, env, gc_inner_root)
	RESULTIS new
      }
    CASE t_hmx:
      { LET gc_inner_root = cons(ast, gc_root)
        RESULTIS alloc_hmx(ast!hmx_key, EVAL(ast!hmx_value, env, gc_inner_root))
      }
    CASE t_hmi:
      { LET gc_inner_root = alloc_vecn(3, ast, env, gc_root)
        LET left  = eval_ast(ast!hmi_left,  env, gc_inner_root)
	LET right = eval_ast(ast!hmi_right, env, cons(left, gc_inner_root))
	RESULTIS alloc_hmi(hmi_critbit OF ast, left, right)
      }
    DEFAULT: RESULTIS ast
  }

AND EVAL(ast, env, gc_root) = VALOF
{ MANIFEST { fun_binds = fun_data; fun_body; fun_env; fun_sz }
  LET gc_inner_root = alloc_vecn(3, ast, env, gc_root)
  gc(gc_inner_root)
  UNLESS type OF ast = t_lst RESULTIS eval_ast(ast, env, gc_root)
  ast := macroexpand(ast, env, gc_inner_root)
  (gc_inner_root+vec_data)!0 := ast // In case it has changed.
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
      gc_inner_root := alloc_vecn(3, newenv, bindings, gc_inner_root)
      UNTIL bindings = empty DO
      { env_set(newenv, bindings!lst_first,
                EVAL(nth(bindings, 1), newenv, gc_inner_root))
        bindings := bindings!lst_rest!lst_rest
      }
      ast, env := nth(ast, 2), newenv
      LOOP // TCO
    }
    IF is_sym(fn, "quote") RESULTIS ast!lst_rest!lst_first
    IF is_sym(fn, "quasiquote") THEN
    { ast := quasiquote(ast!lst_rest!lst_first)
      LOOP // TCO
    }
    IF is_sym(fn, "defmacro!") THEN
    { LET val = EVAL(nth(ast, 2), env, gc_inner_root)
      IF type OF val = t_mfn THEN mfn_ismacro OF val := 1
      env_set(env, nth(ast, 1), val, env)
      RESULTIS val
    }
    IF is_sym(fn, "macroexpand") THEN
      RESULTIS macroexpand(ast!lst_rest!lst_first, env, gc_root)
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
  ast := eval_ast(ast, env, gc_root)
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

LET rep(x) = PRINT(EVAL(READ(x), repl_env, nil))

LET repl(argv) BE
{ LET mal_eval(fn, args, gc_root) = EVAL(args!lst_first, repl_env, gc_root)
  catch_level, catch_label := level(), uncaught_exit
  repl_env := core_env()
  env_set(repl_env, as_sym(str_bcpl2mal("eval")), alloc_fun(mal_eval, fun_data))
  env_set(repl_env, as_sym(str_bcpl2mal("**FILE**")), argv!lst_first)
  env_set(repl_env, as_sym(str_bcpl2mal("**ARGV**")), argv!lst_rest)
  rep(str_bcpl2mal("(def! not (fn** (a) (if a false true)))"), repl_env)
  rep(str_bcpl2mal("(def! load-file (fn** (f) (eval (read-string *
                    *(str *"(do *" (slurp f) *"*nnil)*")))))"), repl_env)
  rep(str_bcpl2mal("(defmacro! cond (fn** (& xs) (if (> (count xs) 0) *
                    *(list 'if (first xs) (if (> (count xs) 1) (nth xs 1) *
                    *(throw *"odd number of forms to cond*")) *
                    *(cons 'cond (rest (rest xs)))))))"), repl_env)
  UNLESS argv = empty DO
  {
    rep(str_bcpl2mal("(load-file **FILE**)"))
    sys(Sys_quit, 0)
    uncaught_exit:
    writes("Uncaught exception: ")
    writes(@(pr_str(last_exception)!str_data))
    newline()
    sys(Sys_quit, 0)
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
    gc(repl_env)
  } REPEAT
}

// This is a cut-down version of the reader's tokenize function.

// Cintsys passes us the entire command line as a single string and doesn't
// quote values in any way, so we can't reliably reconstruct arguments with
// whitespace in them.
LET read_argv() = VALOF
{ LET s = readline(str_bcpl2mal(""))
  LET tokens, tailp = empty, @tokens
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
    !tailp := cons(token, empty)
    tailp := @(!tailp)!lst_rest
  }
  RESULTIS tokens
}

LET start() = VALOF
{ init_types()
  wrch('*n') // Terminate prompt printed by Cintsys
  repl(read_argv())
  RESULTIS 0
}
