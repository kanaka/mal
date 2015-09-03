defmodule Mal.Printer do
  def print_str(mal) when is_atom(mal), do: Atom.to_string(mal)
  def print_str(mal) when is_integer(mal), do: Integer.to_string(mal)
  def print_str(mal) when is_bitstring(mal), do: mal
  def print_str({ :symbol, value }), do: value
  def print_str(mal) when is_list(mal) do
    output = mal
      |> Enum.map(fn(x) -> print_str(x) end)
      |> Enum.join(" ")

    "(#{output})"
  end
end
