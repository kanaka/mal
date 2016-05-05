entity step2_eval is
end entity step2_eval;

library STD;
use STD.textio.all;
library WORK;
use WORK.pkg_readline.all;
use WORK.types.all;
use WORK.printer.all;
use WORK.reader.all;

architecture test of step2_eval is
  procedure mal_READ(str: in string; ast: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    read_str(str, ast, err);
  end procedure mal_READ;

  -- Forward declaration
  procedure EVAL(ast: inout mal_val_ptr; env: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr);

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

  procedure eval_ast_seq(ast_seq: inout mal_seq_ptr; env: inout mal_val_ptr; result: inout mal_seq_ptr; err: out mal_val_ptr) is
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

  procedure eval_ast(ast: inout mal_val_ptr; env: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable key, val, eval_err: mal_val_ptr;
    variable new_seq: mal_seq_ptr;
    variable i: integer;
  begin
    case ast.val_type is
      when mal_symbol =>
        new_string(ast.string_val, key);
        hashmap_get(env, key, val);
        if val = null then
          new_string("'" & ast.string_val.all & "' not found", err);
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

  procedure EVAL(ast: inout mal_val_ptr; env: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a, call_args, sub_err: mal_val_ptr;
  begin
    if ast.val_type /= mal_list then
      eval_ast(ast, env, result, err);
      return;
    end if;

    if ast.seq_val'length = 0 then
      result := ast;
      return;
    end if;

    eval_ast(ast, env, a, sub_err);
    if sub_err /= null then
      err := sub_err;
      return;
    end if;
    seq_drop_prefix(a, 1, call_args);
    eval_native_func(a.seq_val(0), call_args, result);
  end procedure EVAL;

  procedure mal_PRINT(exp: inout mal_val_ptr; result: out line) is
  begin
    pr_str(exp, true, result);
  end procedure mal_PRINT;

  procedure REP(str: in string; env: inout mal_val_ptr; result: out line; err: out mal_val_ptr) is
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
    variable repl_seq: mal_seq_ptr;
    variable repl_env, err: mal_val_ptr;
  begin
    repl_seq := new mal_seq(0 to 7);
    new_string("+", repl_seq(0));
    new_nativefn("+", repl_seq(1));
    new_string("-", repl_seq(2));
    new_nativefn("-", repl_seq(3));
    new_string("*", repl_seq(4));
    new_nativefn("*", repl_seq(5));
    new_string("/", repl_seq(6));
    new_nativefn("/", repl_seq(7));
    new_seq_obj(mal_hashmap, repl_seq, repl_env);

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
