defmodule Mix.Tasks.Step7Quote do
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

    Mal.Env.set(env, "eval", fn [ast] ->
      eval(ast, env)
    end)

    case args do
      [_file_name | rest] -> Mal.Env.set(env, "*ARGV*", rest)
      [] -> Mal.Env.set(env, "*ARGV*", [])
    end
  end

  defp main(env) do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print(env)

    main(env)
  end

  defp eval_ast(ast, env) when is_list(ast) do
    Enum.map(ast, fn elem -> eval(elem, env) end)
  end

  defp eval_ast(ast, env) when is_map(ast) do
    for {key, value} <- ast, into: %{} do
      {eval(key, env), eval(value, env)}
    end
  end

  defp eval_ast({:vector, ast}, env) do
    {:vector, Enum.map(ast, fn elem -> eval(elem, env) end)}
  end

  defp eval_ast({:symbol, symbol}, env) do
    case Mal.Env.get(env, symbol) do
      {:ok, value} -> value
      :not_found -> throw({:error, "invalid symbol #{symbol}"})
    end
  end

  defp eval_ast(ast, _env), do: ast

  defp read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval_bindings({:vector, vector}, env), do: eval_bindings(vector, env)
  defp eval_bindings([], _env), do: _env
  defp eval_bindings([{:symbol, key}, binding | tail], env) do
    evaluated = eval(binding, env)
    Mal.Env.set(env, key, evaluated)
    eval_bindings(tail, env)
  end
  defp eval_bindings(_bindings, _env), do: throw({:error, "Unbalanced let* bindings"})

  defp quasiquote({:vector, list}, _env), do: quasiquote(list, _env)
  defp quasiquote(ast, _env) when not is_list(ast), do: [{:symbol, "quote"}, ast]
  defp quasiquote([], _env), do: [{:symbol, "quote"}, []]
  defp quasiquote([{:symbol, "unquote"}, arg], _env), do: arg
  defp quasiquote([[{:symbol, "splice-unquote"}, first] | tail], env) do
    [{:symbol, "concat"}, first, quasiquote(tail, env)]
  end
  defp quasiquote([head | tail], env) do
    [{:symbol, "cons"}, quasiquote(head, env), quasiquote(tail, env)]
  end

  defp eval([{:symbol, "if"}, condition, if_true | if_false], env) do
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

  defp eval([{:symbol, "do"} | ast], env) do
    eval_ast(List.delete_at(ast, -1), env)
    eval(List.last(ast), env)
  end

  defp eval([{:symbol, "def!"}, {:symbol, key}, value], env) do
    evaluated = eval(value, env)
    Mal.Env.set(env, key, evaluated)
    evaluated
  end

  defp eval([{:symbol, "let*"}, bindings, body], env) do
    let_env = Mal.Env.initialize(env)
    eval_bindings(bindings, let_env)
    eval(body, let_env)
  end

  defp eval([{:symbol, "fn*"}, {:vector, params}, body], env) do
    eval([{:symbol, "fn*"}, params, body], env)
  end
  defp eval([{:symbol, "fn*"}, params, body], env) do
    param_symbols = for {:symbol, symbol} <- params, do: symbol

    closure = fn args ->
      inner = Mal.Env.initialize(env, param_symbols, args)
      eval(body, inner)
    end

    {:closure, closure}
  end

  defp eval([{:symbol, "quote"}, arg], _env), do: arg

  defp eval([{:symbol, "quasiquote"}, ast], env) do
    quasiquote(ast, env)
      |> eval(env)
  end

  defp eval(ast, env) when is_list(ast) do
    [func | args] = eval_ast(ast, env)
    case func do
      {:closure, closure} -> closure.(args)
      _ -> func.(args)
    end
  end

  defp eval(ast, env), do: eval_ast(ast, env)

  defp print(value) do
    IO.puts(Mal.Printer.print_str(value))
  end

  defp read_eval_print(:eof, _env), do: exit(:normal)
  defp read_eval_print(line, env) do
    read(line)
      |> eval(env)
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
