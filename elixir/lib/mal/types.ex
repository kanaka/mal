defmodule Mal.Types do
  def integer?(input) do
    Regex.match?(~r/^-?[0-9]+$/, input)
  end

  def float?(input) do
    Regex.match?(~r/^-?[0-9][0-9.]*$/, input)
  end
end
