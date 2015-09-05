defmodule Mix.Tasks.Step4IfFnDo do
  def run(_) do
    env = Mal.Env.initialize()
    Mal.Env.merge(env, Mal.Core.namespace)
    main(env)
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

  defp eval_if_false([], _env), do: nil
  defp eval_if_false([body], env), do: eval(body, env)

  def eval([{:symbol, "if"}, condition, if_true | if_false], env) do
    result = eval(condition, env)
    if result == nil or result == false do
      eval_if_false(if_false, env)
    else
      eval(if_true, env)
    end
  end

  def eval([{:symbol, "do"} | ast], env) do
    eval_ast(ast, env)
      |> List.last
  end

  def eval([{:symbol, "def!"}, {:symbol, key}, value], env) do
    evaluated = eval(value, env)
    Mal.Env.set(env, key, evaluated)
    evaluated
  end

  def eval([{:symbol, "let*"}, bindings, body], env) do
    let_env = Mal.Env.initialize(env)
    eval_bindings(bindings, let_env)
    eval(body, let_env)
  end

  def eval([{:symbol, "fn*"}, params, body], env) do
    param_symbols = for {:symbol, symbol} <- params, do: symbol

    closure = fn args ->
      inner = Mal.Env.initialize(env, param_symbols, args)
      eval(body, inner)
    end

    {:closure, closure}
  end

  def eval(ast, env) when is_list(ast) do
    [func | args] = eval_ast(ast, env)
    case func do
      {:closure, closure} -> closure.(args)
      _ -> func.(args)
    end
  end

  def eval(ast, env), do: eval_ast(ast, env)

  def print(value) do
    IO.puts(Mal.Printer.print_str(value))
  end

  def read_eval_print(:eof, _env), do: exit(0)
  def read_eval_print(line, env) do
    read(line)
      |> eval(env)
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
