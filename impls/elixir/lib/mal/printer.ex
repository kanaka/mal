defmodule Mal.Printer do
  alias Mal.Function

  def print_str(mal, print_readably \\ true)
  def print_str(mal, _) when is_atom(mal), do: inspect(mal)
  def print_str(mal, _) when is_integer(mal), do: Integer.to_string(mal)
  def print_str(mal, _) when is_function(mal), do: inspect(mal)
  def print_str(%Function{value: mal, macro: true}, _), do: "#Macro<#{inspect(mal)}"
  def print_str(%Function{value: mal}, _), do: inspect(mal)
  def print_str({:symbol, value}, _), do: value
  def print_str({:exception, exception}, print_readably) do
    print_str(exception, print_readably)
  end
  def print_str(mal, false) when is_bitstring(mal), do: mal
  def print_str(mal, true) when is_bitstring(mal), do: inspect(mal)

  def print_str({:atom, _pid} = atom, print_readably) do
    output = atom
      |> Mal.Atom.deref
      |> print_str(print_readably)

    "(atom #{output})"
  end

  def print_str({:map, mal, _}, print_readably) do
    evaluate_pair = fn {key, value} ->
      "#{print_str(key, print_readably)} #{print_str(value, print_readably)}"
    end

    output = mal
      |> Enum.map(evaluate_pair)
      |> Enum.join(" ")

    "{#{output}}"
  end

  def print_str({:vector, vector, _}, print_readably) do
    "[#{print_list(vector, print_readably)}]"
  end

  def print_str({:list, mal, _}, print_readably) do
    "(#{print_list(mal, print_readably)})"
  end

  defp print_list(list, print_readably) do
    list
      |> Enum.map(fn(x) -> print_str(x, print_readably) end)
      |> Enum.join(" ")
  end
end
