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

  def eval({:ok, input}), do: {:ok, input}
  def eval({:error, message}), do: {:error, message}

  def print({:ok, output}) do
    IO.puts(Mal.Printer.print_str(output))
  end
  def print({:error, message}) do
    IO.puts(message)
  end

  def read_eval_print(:eof), do: exit(0)
  def read_eval_print(line) do
    read(line)
      |> eval
      |> print
  end
end
