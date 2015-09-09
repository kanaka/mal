defmodule Mal.Types do
  def integer?(input) do
    Regex.match?(~r/^-?[0-9]+$/, input)
  end

  def hash_map(ast) do
    map = ast
      |> Enum.chunk(2)
      |> Enum.map(&List.to_tuple/1)
      |> Enum.into(%{})

    {:map, map, nil}
  end

  def map?([{:map, _ast, _meta}]), do: true
  def map?(_), do: false

  def list(ast), do: {:list, ast, nil}

  def list?([{:list, _, _}]), do: true
  def list?(_), do: false

  def vector(ast), do: {:vector, ast, nil}

  def vector?([{:vector, _ast, _meta}]), do: true
  def vector?(_), do: false

  def symbol?([{:symbol, _}]), do: true
  def symbol?(_), do: false

  def atom([value]) do
    pid = Mal.Atom.new(value)
    {:atom, pid}
  end

  def atom?([{:atom, _}]), do: true
  def atom?(_), do: false
end

defmodule Mal.Function do
  defstruct value: nil, macro: false, meta: nil
end
