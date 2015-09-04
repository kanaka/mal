defmodule Mix.Tasks.Step1ReadPrint do
  def run(_), do: main

  def main do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print

    main
  end

  def read(input) do
    Mal.Reader.read_str(input)
  end

  def eval(ast), do: ast

  def print(value) do
    IO.puts(Mal.Printer.print_str(value))
  end

  def read_eval_print(:eof), do: exit(0)
  def read_eval_print(line) do
    read(line)
      |> eval
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
