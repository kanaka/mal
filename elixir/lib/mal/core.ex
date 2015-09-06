defmodule Mal.Core do
  def namespace do
    %{
      "+" => fn [a, b] -> a + b end,
      "-" => fn [a, b] -> a - b end,
      "*" => fn [a, b] -> a * b end,
      "/" => fn [a, b] -> div(a, b) end,
      "=" => fn [a, b] -> a == b end,
      ">" => fn [a, b] -> a > b end,
      "<" => fn [a, b] -> a < b end,
      "<=" => fn [a, b] -> a <= b end,
      ">=" => fn [a, b] -> a >= b end,
      "list" => fn args -> args end,
      "list?" => &list?/1,
      "empty?" => &empty?/1,
      "count" => &count/1,
      "pr-str" => &pr_str/1,
      "str" => &str/1,
      "prn" => &prn/1,
      "println" => &println/1,
      "read-string" => fn [input] -> Mal.Reader.read_str(input) end,
      "slurp" => &slurp/1
    }
  end

  def list?([arg]) when is_list(arg), do: true
  def list?([_arg]), do: false

  def empty?([[]]), do: true
  def empty?(_), do: false

  def count([arg]) when is_list(arg), do: length(arg)
  def count(_), do: 0

  def pr_str(args) do
    args
      |> Enum.map(&Mal.Printer.print_str/1)
      |> Enum.join(" ")
  end

  def str(args) do
    args
      |> Enum.map(&(Mal.Printer.print_str(&1, false)))
      |> Enum.join("")
  end

  def prn(args) do
    args
      |> pr_str
      |> IO.puts
    nil
  end

  def println(args) do
    args
      |> Enum.map(&(Mal.Printer.print_str(&1, false)))
      |> Enum.join(" ")
      |> IO.puts
    nil
  end

  def slurp([file_name]) do
    case File.read(file_name) do
      {:ok, content} -> content
      {:error, :enoent} -> throw({:error, "can't find file #{file_name}"})
      {:error, :eisdir} -> throw({:error, "can't read directory #{file_name}"})
      {:error, :eaccess} -> throw({:error, "missing permissions #{file_name}"})
      {:error, reason} -> throw({:error, "can't read file #{file_name}, #{reason}"})
    end
  end
end
