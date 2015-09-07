defmodule Mal.Printer do
  def print_str(mal, print_readably \\ true)
  def print_str(mal, _) when is_atom(mal), do: inspect(mal)
  def print_str(mal, _) when is_integer(mal), do: Integer.to_string(mal)
  def print_str(mal, _) when is_function(mal), do: inspect(mal)
  def print_str({:closure, mal}, _), do: inspect(mal)
  def print_str({:macro, mal}, _), do: "#Macro<#{inspect(mal)}"
  def print_str({:symbol, value}, _), do: value
  def print_str({:exception, exception}, print_readably) do
    print_str(exception, print_readably)
  end
  def print_str(mal, false) when is_bitstring(mal), do: mal
  def print_str(mal, true) when is_bitstring(mal), do: inspect(mal)

  def print_str({:vector, vector}, print_readably) do
    "[#{print_list(vector, print_readably)}]"
  end

  def print_str(mal, print_readably) when is_list(mal) do
    "(#{print_list(mal, print_readably)})"
  end

  defp print_list(list, print_readably) do
    list
      |> Enum.map(fn(x) -> print_str(x, print_readably) end)
      |> Enum.join(" ")
  end
end
