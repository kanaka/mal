defmodule Mal.Types do
  def integer?(input) do
    Regex.match?(~r/^-?[0-9]+$/, input)
  end

  def float?(input) do
    Regex.match?(~r/^-?[0-9][0-9.]*$/, input)
  end

  def hash_map(ast) do
    ast
      |> Enum.chunk(2)
      |> Enum.map(&List.to_tuple/1)
      |> Enum.into(%{})
  end
end
