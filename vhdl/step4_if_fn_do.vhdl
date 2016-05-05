entity step4_if_fn_do is
end entity step4_if_fn_do;

library STD;
use STD.textio.all;
library WORK;
use WORK.pkg_readline.all;
use WORK.types.all;
use WORK.printer.all;
use WORK.reader.all;
use WORK.env.all;
use WORK.core.all;

architecture test of step4_if_fn_do is
  procedure mal_READ(str: in string; ast: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    read_str(str, ast, err);
  end procedure mal_READ;

  -- Forward declaration
  procedure EVAL(ast: inout mal_val_ptr; env: inout env_ptr; result: out mal_val_ptr; err: out mal_val_ptr);

  procedure eval_ast_seq(ast_seq: inout mal_seq_ptr; env: inout env_ptr; result: inout mal_seq_ptr; err: out mal_val_ptr) is
    variable eval_err: mal_val_ptr;
  begin
    result := new mal_seq(0 to ast_seq'length - 1);
    for i in result'range loop
      EVAL(ast_seq(i), env, result(i), eval_err);
      if eval_err /= null then
        err := eval_err;
        return;
      end if;
    end loop;
  end procedure eval_ast_seq;

  procedure eval_ast(ast: inout mal_val_ptr; env: inout env_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable key, val, eval_err, env_err: mal_val_ptr;
    variable new_seq: mal_seq_ptr;
    variable i: integer;
  begin
    case ast.val_type is
      when mal_symbol =>
        env_get(env, ast, val, env_err);
        if env_err /= null then
          err := env_err;
          return;
        end if;
        result := val;
        return;
      when mal_list | mal_vector | mal_hashmap =>
        eval_ast_seq(ast.seq_val, env, new_seq, eval_err);
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
  end procedure eval_ast;

  procedure EVAL(ast: inout mal_val_ptr; env: inout env_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable i: integer;
    variable evaled_ast, a0, call_args, val, vars, sub_err, fn: mal_val_ptr;
    variable let_env, fn_env: env_ptr;
  begin
    if ast.val_type /= mal_list then
      eval_ast(ast, env, result, err);
      return;
    end if;

    if ast.seq_val'length = 0 then
      result := ast;
      return;
    end if;

    a0 := ast.seq_val(0);
    if a0.val_type = mal_symbol then
      if a0.string_val.all = "def!" then
        EVAL(ast.seq_val(2), env, val, sub_err);
        if sub_err /= null then
          err := sub_err;
          return;
        end if;
        env_set(env, ast.seq_val(1), val);
        result := val;
        return;

      elsif a0.string_val.all = "let*" then
        vars := ast.seq_val(1);
        new_env(let_env, env);
        i := 0;
        while i < vars.seq_val'length loop
          EVAL(vars.seq_val(i + 1), let_env, val, sub_err);
          if sub_err /= null then
            err := sub_err;
            return;
          end if;
          env_set(let_env, vars.seq_val(i), val);
          i := i + 2;
        end loop;
        EVAL(ast.seq_val(2), let_env, result, err);
        return;

      elsif a0.string_val.all = "do" then
        for i in 1 to ast.seq_val'high loop
          EVAL(ast.seq_val(i), env, result, sub_err);
          if sub_err /= null then
            err := sub_err;
            return;
          end if;
        end loop;
        return;

      elsif a0.string_val.all = "if" then
        EVAL(ast.seq_val(1), env, val, sub_err);
        if sub_err /= null then
          err := sub_err;
          return;
        end if;
        if val.val_type = mal_nil or val.val_type = mal_false then
          if ast.seq_val'length > 3 then
            EVAL(ast.seq_val(3), env, result, err);
          else
            new_nil(result);
          end if;
        else
          EVAL(ast.seq_val(2), env, result, err);
        end if;
        return;

      elsif a0.string_val.all = "fn*" then
        new_fn(ast.seq_val(2), ast.seq_val(1), env, result);
        return;

      end if;
    end if;

    eval_ast(ast, env, evaled_ast, sub_err);
    if sub_err /= null then
      err := sub_err;
      return;
    end if;
    seq_drop_prefix(evaled_ast, 1, call_args);
    fn := evaled_ast.seq_val(0);
    case fn.val_type is
      when mal_nativefn =>
        eval_native_func(fn, call_args, result, err);
      when mal_fn => 
        new_env(fn_env, fn.func_val.f_env, fn.func_val.f_args, call_args);
        EVAL(fn.func_val.f_body, fn_env, result, err);
      when others =>
        new_string("not a function", err);
    end case;
  end procedure EVAL;

  procedure mal_PRINT(exp: inout mal_val_ptr; result: out line) is
  begin
    pr_str(exp, true, result);
  end procedure mal_PRINT;

  procedure RE(str: in string; env: inout env_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable ast, read_err: mal_val_ptr;
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
    EVAL(ast, env, result, err);
  end procedure RE;

  procedure REP(str: in string; env: inout env_ptr; result: out line; err: out mal_val_ptr) is
    variable eval_res, eval_err: mal_val_ptr;
  begin
    RE(str, env, eval_res, eval_err);
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
    variable dummy_val, err: mal_val_ptr;
    variable outer, repl_env: env_ptr;
  begin
    outer := null;
    new_env(repl_env, outer);

    -- core.EXT: defined using VHDL (see core.vhdl)
    define_core_functions(repl_env);

    -- core.mal: defined using the language itself
    RE("(def! not (fn* (a) (if a false true)))", repl_env, dummy_val, err);

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
