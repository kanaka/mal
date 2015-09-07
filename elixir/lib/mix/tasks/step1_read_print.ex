defmodule Mix.Tasks.Step1ReadPrint do
  def run(_), do: loop

  defp loop do
    Mal.Core.readline("user> ")
      |> read_eval_print
      |> IO.puts

    loop
  end

  defp read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval(ast), do: ast

  defp print(value) do
    Mal.Printer.print_str(value)
  end

  defp read_eval_print(:eof), do: exit(:normal)
  defp read_eval_print(line) do
    read(line)
      |> eval
      |> print
  catch
    {:error, message} -> IO.puts("Error: #{message}")
  end
end
