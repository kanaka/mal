defmodule Mix.Tasks.Step3Env do
  @initial_env %{
    "+" => &+/2,
    "-" => &-/2,
    "*" => &*/2,
    "/" => &div/2
  }

  def run(_) do
    env = Mal.Env.new()
    Mal.Env.merge(env, @initial_env)
    loop(env)
  end

  defp loop(env) do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print(env)
      |> IO.puts

    loop(env)
  end

  defp eval_ast({:list, ast, meta}, env) when is_list(ast) do
    {:list, Enum.map(ast, fn elem -> eval(elem, env) end), meta}
  end

  defp eval_ast({:map, ast, meta}, env) do
    map = for {key, value} <- ast, into: %{} do
      {eval(key, env), eval(value, env)}
    end

    {:map, map, meta}
  end

  defp eval_ast({:vector, ast, meta}, env) do
    {:vector, Enum.map(ast, fn elem -> eval(elem, env) end), meta}
  end

  defp eval_ast({:symbol, symbol}, env) do
    case Mal.Env.get(env, symbol) do
      {:ok, value} -> value
      :not_found -> throw({:error, "'#{symbol}' not found"})
    end
  end

  defp eval_ast(ast, _env), do: ast

  defp read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval_bindings([], _env), do: _env
  defp eval_bindings([{:symbol, key}, binding | tail], env) do
    evaluated = eval(binding, env)
    Mal.Env.set(env, key, evaluated)
    eval_bindings(tail, env)
  end
  defp eval_bindings(_bindings, _env), do: throw({:error, "Unbalanced let* bindings"})

  defp eval({:list, ast, meta}, env), do: eval_list(ast, env, meta)
  defp eval(ast, env), do: eval_ast(ast, env)

  defp eval_list([{:symbol, "def!"}, {:symbol, key}, value], env, _) do
    evaluated = eval(value, env)
    Mal.Env.set(env, key, evaluated)
    evaluated
  end

  defp eval_list([{:symbol, "let*"}, {list_type, bindings, _}, body], env, _)
  when list_type == :list or list_type == :vector do
    let_env = Mal.Env.new(env)
    eval_bindings(bindings, let_env)
    eval(body, let_env)
  end

  defp eval_list(ast, env, meta) do
    {:list, [func | args], _} = eval_ast({:list, ast, meta}, env)
    apply(func, args)
  end

  defp print(value) do
    Mal.Printer.print_str(value)
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
