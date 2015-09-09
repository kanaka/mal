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

  def list(ast), do: {:list, ast, nil}

  def vector(ast), do: {:vector, ast, nil}
end

defmodule Mal.Function do
  defstruct value: nil, macro: false, meta: nil 
end
