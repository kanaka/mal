defmodule Mix.Tasks.Step0Repl do
  def run(_), do: main

  def main do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> handle_line

    main
  end

  defp handle_line(:eof), do: exit(:normal)
  defp handle_line(line) do
    IO.write(:stdio, read_eval_print(line))
  end

  def read(input) do
    input
  end

  def eval(input) do
    input
  end

  def print(input) do
    input
  end

  def read_eval_print(line) do
    read(line)
      |> eval
      |> print
  end
end
