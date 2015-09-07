defmodule Mix.Tasks.Step1ReadPrint do
  def run(_), do: main

  defp main do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print

    main
  end

  defp read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval(ast), do: ast

  defp print(value) do
    IO.puts(Mal.Printer.print_str(value))
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
