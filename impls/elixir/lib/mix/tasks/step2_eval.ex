defmodule Mix.Tasks.Step2Eval do
  @repl_env %{
    "+" => &+/2,
    "-" => &-/2,
    "*" => &*/2,
    "/" => &div/2
  }

  def run(_), do: loop()

  defp loop do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print
      |> IO.puts

    loop()
  end

  defp eval_ast({:list, ast, meta}, env) when is_list(ast) do
    eval_list(ast, env, meta)
  end

  defp eval_ast({:map, ast, meta}, env) do
    map = for {key, value} <- ast, into: %{} do
      {key, eval(value, env)}
    end

    {:map, map, meta}
  end

  defp eval_ast({:vector, ast, meta}, env) do
    {:vector, Enum.map(ast, fn elem -> eval(elem, env) end), meta}
  end

  defp eval_ast({:symbol, symbol}, env) do
    case Map.fetch(env, symbol) do
      {:ok, value} -> value
      :error -> throw({:error, "'#{symbol}' not found"})
    end
  end

  defp eval_ast(ast, _env), do: ast

  defp read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval(ast, env) do
    # IO.puts("EVAL: #{Mal.Printer.print_str(ast)}")
    eval_ast(ast, env)
  end

  defp eval_list([a0 | args], env, _meta) do
    func = eval(a0, env)
    args = Enum.map(args, fn elem -> eval(elem, env) end)
    apply(func, args)
  end

  defp eval_list([], _env, meta), do: {:list, [], meta}

  defp print(value) do
    Mal.Printer.print_str(value)
  end

  defp read_eval_print(:eof), do: exit(:normal)
  defp read_eval_print(line) do
    read(line)
      |> eval(@repl_env)
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
