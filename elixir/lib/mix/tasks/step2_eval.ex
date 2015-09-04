defmodule Mix.Tasks.Step2Eval do
  @repl_env %{
    "+" => &+/2,
    "-" => &-/2,
    "*" => &*/2,
    "/" => &div/2
  }

  def run(_), do: main

  def main do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print

    main
  end

  def eval_ast(ast, env) when is_list(ast) do
    Enum.map(ast, fn elem -> eval(elem, env) end)
  end

  def eval_ast({:symbol, symbol}, env) do
    case Map.fetch(env, symbol) do
      {:ok, value} -> value
      :error -> throw({:error, "invalid symbol #{symbol}"})
    end
  end

  def eval_ast(ast, _env), do: ast

  def read(input) do
    Mal.Reader.read_str(input)
  end

  def eval(ast, env) when is_list(ast) do
    [func | args] = eval_ast(ast, env)
    apply(func, args)
  end

  def eval(ast, env), do: eval_ast(ast, env)

  def print(value) do
    IO.puts(Mal.Printer.print_str(value))
  end

  def read_eval_print(:eof), do: exit(0)
  def read_eval_print(line) do
    read(line)
      |> eval(@repl_env)
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
