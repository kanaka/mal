defmodule Mix.Tasks.Step8Macros do
  def run(args) do
    env = Mal.Env.initialize()
    Mal.Env.merge(env, Mal.Core.namespace)
    bootstrap(args, env)
    load_file(args, env)
    main(env)
  end

  defp load_file([], _env), do: nil
  defp load_file([file_name | _args], env) do
    read_eval_print("""
      (load-file "#{file_name}")
      """, env)
    exit(:normal)
  end

  defp bootstrap(args, env) do
    # not:
    read_eval_print("""
      (def! not
        (fn* (a) (if a false true)))
      """, env)

    # load-file:
    read_eval_print("""
      (def! load-file
        (fn* (f)
          (eval (read-string (str "(do " (slurp f) ")")))))
      """, env)

    # cond
    read_eval_print("""
      (defmacro! cond
        (fn* (& xs)
          (if (> (count xs) 0)
            (list 'if (first xs)
              (if (> (count xs) 1)
                (nth xs 1)
                (throw \"odd number of forms to cond\"))
              (cons 'cond (rest (rest xs)))))))"
      """, env)

    # or:
    read_eval_print("""
      (defmacro! or
        (fn* (& xs)
          (if (empty? xs)
            nil
            (if (= 1 (count xs))
              (first xs)
              `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))
      """, env)

    Mal.Env.set(env, "eval", fn [ast] ->
      eval(ast, env)
    end)

    case args do
      [_file_name | rest] -> Mal.Env.set(env, "*ARGV*", rest)
      [] -> Mal.Env.set(env, "*ARGV*", [])
    end
  end

  def main(env) do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print(env)

    main(env)
  end

  def eval_ast(ast, env) when is_list(ast) do
    Enum.map(ast, fn elem -> eval(elem, env) end)
  end

  def eval_ast({:symbol, symbol}, env) do
    case Mal.Env.get(env, symbol) do
      {:ok, value} -> value
      :not_found -> throw({:error, "invalid symbol #{symbol}"})
    end
  end

  def eval_ast(ast, _env), do: ast

  def read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval_bindings([], _env), do: _env
  defp eval_bindings([{:symbol, key}, binding | tail], env) do
    evaluated = eval(binding, env)
    Mal.Env.set(env, key, evaluated)
    eval_bindings(tail, env)
  end
  defp eval_bindings(_bindings, _env), do: throw({:error, "Unbalanced let* bindings"})

  defp quasiquote(ast, _env) when not is_list(ast), do: [{:symbol, "quote"}, ast]
  defp quasiquote([], _env), do: [{:symbol, "quote"}, []]
  defp quasiquote([{:symbol, "unquote"}, arg], _env), do: arg
  defp quasiquote([[{:symbol, "splice-unquote"}, first] | tail], env) do
    [{:symbol, "concat"}, first, quasiquote(tail, env)]
  end
  defp quasiquote([head | tail], env) do
    [{:symbol, "cons"}, quasiquote(head, env), quasiquote(tail, env)]
  end

  defp macro_call?([{:symbol, key} | _tail], env) do
    case Mal.Env.get(env, key) do
      {:ok, {:macro, _}} -> true
      _ -> false
    end
  end
  defp macro_call?(_ast, _env), do: false

  defp do_macro_call([{:symbol, key} | tail], env) do
    {:ok, {:macro, macro}} = Mal.Env.get(env, key)
    macro.(tail)
      |> macroexpand(env)
  end

  def macroexpand(ast, env) do
    if macro_call?(ast, env) do
      do_macro_call(ast, env)
    else
      ast
    end
  end

  def eval(ast, env) when not is_list(ast), do: eval_ast(ast, env)
  def eval(ast, env) when is_list(ast) do
    case macroexpand(ast, env) do
      result when is_list(result) -> eval_list(result, env)
      result -> result
    end
  end

  def eval_list([{:symbol, "macroexpand"}, ast], env), do: macroexpand(ast, env)

  def eval_list([{:symbol, "if"}, condition, if_true | if_false], env) do
    result = eval(condition, env)
    if result == nil or result == false do
      case if_false do
        [] -> nil
        [body] -> eval(body, env)
      end
    else
      eval(if_true, env)
    end
  end

  def eval_list([{:symbol, "do"} | ast], env) do
    eval_ast(List.delete_at(ast, -1), env)
    eval(List.last(ast), env)
  end

  def eval_list([{:symbol, "def!"}, {:symbol, key}, value], env) do
    evaluated = eval(value, env)
    Mal.Env.set(env, key, evaluated)
    evaluated
  end

  def eval_list([{:symbol, "defmacro!"}, {:symbol, key}, function], env) do
    {:closure, evaluated} = eval(function, env)
    macro = {:macro, evaluated}
    Mal.Env.set(env, key, macro)
    macro
  end

  def eval_list([{:symbol, "let*"}, bindings, body], env) do
    let_env = Mal.Env.initialize(env)
    eval_bindings(bindings, let_env)
    eval(body, let_env)
  end

  def eval_list([{:symbol, "fn*"}, params, body], env) do
    param_symbols = for {:symbol, symbol} <- params, do: symbol

    closure = fn args ->
      inner = Mal.Env.initialize(env, param_symbols, args)
      eval(body, inner)
    end

    {:closure, closure}
  end

  def eval_list([{:symbol, "quote"}, arg], _env), do: arg

  def eval_list([{:symbol, "quasiquote"}, ast], env) do
    quasiquote(ast, env)
      |> eval(env)
  end

  def eval_list(ast, env) do
    [func | args] = eval_ast(ast, env)
    case func do
      {:closure, closure} -> closure.(args)
      _ -> func.(args)
    end
  end

  def print(value) do
    IO.puts(Mal.Printer.print_str(value))
  end

  def read_eval_print(:eof, _env), do: exit(:normal)
  def read_eval_print(line, env) do
    read(line)
      |> eval(env)
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
