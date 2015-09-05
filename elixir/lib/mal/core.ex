defmodule Mal.Core do
  def namespace do
    %{
      "+" => &add/1,
      "-" => &subtract/1,
      "*" => &multiply/1,
      "/" => &divide/1,
    }
  end

  def add([first, second]), do: first + second
  def subtract([first, second]), do: first - second
  def multiply([first, second]), do: first * second
  def divide([first, second]), do: div(first, second)
end
