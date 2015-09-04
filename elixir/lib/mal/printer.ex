defmodule Mal.Printer do
  def print_str(mal, print_readably \\ true)
  def print_str(mal, _print_readably) when is_atom(mal), do: Atom.to_string(mal)
  def print_str(mal, _print_readably) when is_integer(mal), do: Integer.to_string(mal)
  def print_str({ :symbol, value }, _print_readably), do: value
  def print_str(mal, false) when is_bitstring(mal), do: mal
  def print_str(mal, true) when is_bitstring(mal) do
    output = String.replace(mal, "\"", "\\\"")
    "\"#{output}\""
   end

  def print_str(mal, _print_readably) when is_list(mal) do
    output = mal
      |> Enum.map(fn(x) -> print_str(x) end)
      |> Enum.join(" ")

    "(#{output})"
  end
end
