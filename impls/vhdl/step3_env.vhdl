entity step3_env is
end entity step3_env;

library STD;
use STD.textio.all;
library WORK;
use WORK.pkg_readline.all;
use WORK.types.all;
use WORK.printer.all;
use WORK.reader.all;
use WORK.env.all;

architecture test of step3_env is
  procedure mal_READ(str: in string; ast: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    read_str(str, ast, err);
  end procedure mal_READ;

  -- Forward declaration
  procedure EVAL(ast: inout mal_val_ptr; env: inout env_ptr; result: out mal_val_ptr; err: out mal_val_ptr);

  procedure eval_native_func(func_sym: inout mal_val_ptr; args: inout mal_val_ptr; result: out mal_val_ptr) is
    variable num_result: integer;
    variable a: mal_seq_ptr;
  begin
    a := args.seq_val;
    if func_sym.string_val.all = "+" then
      new_number(a(0).number_val + a(1).number_val, result);
    elsif func_sym.string_val.all = "-" then
      new_number(a(0).number_val - a(1).number_val, result);
    elsif func_sym.string_val.all = "*" then
      new_number(a(0).number_val * a(1).number_val, result);
    elsif func_sym.string_val.all = "/" then
      new_number(a(0).number_val / a(1).number_val, result);
    else
      result := null;
    end if;
  end procedure eval_native_func;

  procedure eval_ast_seq(ast_seq : inout mal_seq_ptr;
                         skip    : in    natural;
                         env     : inout env_ptr;
                         result  : inout mal_seq_ptr;
                         err     :   out mal_val_ptr) is
    variable eval_err: mal_val_ptr;
  begin
    result := new mal_seq(0 to ast_seq'length - 1 - skip);
    for i in result'range loop
      EVAL(ast_seq(skip + i), env, result(i), eval_err);
      if eval_err /= null then
        err := eval_err;
        return;
      end if;
    end loop;
  end procedure eval_ast_seq;

  procedure EVAL(ast    : inout mal_val_ptr;
                 env    : inout env_ptr;
                 result :   out mal_val_ptr;
                 err    :   out mal_val_ptr) is
    variable val, eval_err, a0, call_args, vars, fn, sub_err: mal_val_ptr;
    variable let_env : env_ptr;
    variable s: line;
    variable new_seq: mal_seq_ptr;
    variable i: integer;
  begin
    new_symbol("DEBUG-EVAL", a0);
    env_get(env, a0, val);
    if val /= null and val.val_type /= mal_nil and val.val_type /= mal_false
    then
      mal_printstr("EVAL: ");
      pr_str(ast, true, s);
      mal_printline(s.all);
    end if;

    case ast.val_type is
      when mal_symbol =>
        env_get(env, ast, val);
        if val = null then
          new_string("'" & ast.string_val.all & "' not found", err);
          return;
        end if;
        result := val;
        return;
      when mal_list =>
        null;
      when mal_vector | mal_hashmap =>
        eval_ast_seq(ast.seq_val, 0, env, new_seq, eval_err);
        if eval_err /= null then
          err := eval_err;
          return;
        end if;
        new_seq_obj(ast.val_type, new_seq, result);
        return;
      when others =>
        result := ast;
        return;
    end case;

    if ast.seq_val'length = 0 then
      result := ast;
      return;
    end if;

    a0 := ast.seq_val(0);
    if a0.string_val.all = "def!" then
      EVAL(ast.seq_val(2), env, val, sub_err);
      if sub_err /= null then
        err := sub_err;
        return;
      end if;
      env_set(env, ast.seq_val(1), val);
      result := val;
    elsif a0.string_val.all = "let*" then
      vars := ast.seq_val(1);
      new_env(let_env, env);
      i := 0;
      while i < vars.seq_val'length loop
        EVAL(vars.seq_val(i + 1), let_env, val, sub_err);
        if sub_err /= null then
          err := sub_err;
          deallocate(let_env);
          return;
        end if;
        env_set(let_env, vars.seq_val(i), val);
        i := i + 2;
      end loop;
      EVAL(ast.seq_val(2), let_env, result, err);
      deallocate(let_env);
    else
      EVAL (a0, env, fn, sub_err);
      if sub_err /= null then
        err := sub_err;
        return;
      end if;
      -- Evaluate arguments
      eval_ast_seq(ast.seq_val, 1, env, new_seq, sub_err);
      if sub_err /= null then
        err := sub_err;
        return;
      end if;
      new_seq_obj(mal_list, new_seq, call_args);
      eval_native_func(fn, call_args, result);
    end if;
  end procedure EVAL;

  procedure mal_PRINT(exp: inout mal_val_ptr; result: out line) is
  begin
    pr_str(exp, true, result);
  end procedure mal_PRINT;

  procedure REP(str: in string; env: inout env_ptr; result: out line; err: out mal_val_ptr) is
    variable ast, eval_res, read_err, eval_err: mal_val_ptr;
  begin
    mal_READ(str, ast, read_err);
    if read_err /= null then
      err := read_err;
      result := null;
      return;
    end if;
    if ast = null then
      result := null;
      return;
    end if;
    EVAL(ast, env, eval_res, eval_err);
    if eval_err /= null then
      err := eval_err;
      result := null;
      return;
    end if;
    mal_PRINT(eval_res, result);
  end procedure REP;

  procedure repl is
    variable is_eof: boolean;
    variable input_line, result: line;
    variable sym, fn, err: mal_val_ptr;
    variable outer, repl_env: env_ptr;
  begin
    outer := null;
    new_env(repl_env, outer);
    new_symbol("+", sym);
    new_nativefn("+", fn);
    env_set(repl_env, sym, fn);
    new_symbol("-", sym);
    new_nativefn("-", fn);
    env_set(repl_env, sym, fn);
    new_symbol("*", sym);
    new_nativefn("*", fn);
    env_set(repl_env, sym, fn);
    new_symbol("/", sym);
    new_nativefn("/", fn);
    env_set(repl_env, sym, fn);

    loop
      mal_readline("user> ", is_eof, input_line);
      exit when is_eof;
      next when input_line'length = 0;
      REP(input_line.all, repl_env, result, err);
      if err /= null then
        pr_str(err, false, result);
        result := new string'("Error: " & result.all);
      end if;
      if result /= null then
        mal_printline(result.all);
      end if;
      deallocate(result);
      deallocate(err);
    end loop;
    mal_printline("");
  end procedure repl;

begin
  repl;
end architecture test;
