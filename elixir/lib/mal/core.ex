defmodule Mal.Core do
  def namespace do
    %{
      "+" => fn [a, b] -> a + b end,
      "-" => fn [a, b] -> a - b end,
      "*" => fn [a, b] -> a * b end,
      "/" => fn [a, b] -> div(a, b) end,
      ">" => fn [a, b] -> a > b end,
      "<" => fn [a, b] -> a < b end,
      "<=" => fn [a, b] -> a <= b end,
      ">=" => fn [a, b] -> a >= b end,
      "concat" => &concat/1,
      "=" => &equal/1,
      "list?" => &list?/1,
      "empty?" => &empty?/1,
      "count" => &count/1,
      "pr-str" => &pr_str/1,
      "str" => &str/1,
      "prn" => &prn/1,
      "println" => &println/1,
      "slurp" => &slurp/1,
      "nth" => &nth/1,
      "first" => &first/1,
      "rest" => &rest/1,
      "map" => &map/1,
      "apply" => &apply/1,
      "keyword" => &keyword/1,
      "symbol?" => &symbol?/1,
      "cons" => &cons/1,
      "vector?" => &vector?/1,
      "sequential?" => fn arg -> vector?(arg) or list?(arg) end,
      "vector" => fn list -> {:vector, list} end,
      "keyword?" => fn [type] -> is_atom(type) end,
      "nil?" => fn [type] -> type == nil end,
      "true?" => fn [type] -> type == true end,
      "false?" => fn [type] -> type == false end,
      "symbol" => fn [name] -> {:symbol, name} end,
      "list" => fn args -> args end,
      "read-string" => fn [input] -> Mal.Reader.read_str(input) end,
      "throw" => fn [arg] -> throw({:error, arg}) end,
    }
  end

  defp convert_vector({:vector, list}), do: list
  defp convert_vector(other), do: other

  def equal([a, b]) do
    convert_vector(a) == convert_vector(b)
  end

  def list?([arg]) when is_list(arg), do: true
  def list?([_arg]), do: false

  def empty?([[]]), do: true
  def empty?([{:vector, []}]), do: true
  def empty?(_), do: false

  def count([arg]) when is_list(arg), do: length(arg)
  def count([{:vector, arg}]), do: length(arg)
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

  def nth([list, index]) do
    case Enum.at(convert_vector(list), index, :error) do
      :error -> throw({:error, "index out of bounds"})
      any -> any
    end
  end

  def first([{:vector, [head | tail]}]), do: head
  def first([[head | tail]]), do: head
  def first(_), do: nil

  def rest([{:vector, list}]), do: do_rest(list)
  def rest([list]), do: do_rest(list)

  defp do_rest([head | tail]), do: tail
  defp do_rest([]), do: []

  def map([{_function_type, function}, list]), do: do_map(function, list)
  def map([function, list]), do: do_map(function, list)

  defp do_map(function, list) do
    convert_vector(list)
      |> Enum.map(fn arg -> function.([arg]) end)
  end

  def apply([{_function_type, function} | tail]), do: do_apply(function, tail)
  def apply([function | tail]), do: do_apply(function, tail)

  def do_apply(function, tail) do
    [list | reversed_args] = Enum.reverse(tail)
    args = Enum.reverse(reversed_args)
    func_args = Enum.concat(args, convert_vector(list))
    function.(func_args)
  end

  def symbol?([{:symbol, _}]), do: true
  def symbol?(_), do: false

  def vector?([{:vector, _}]), do: true
  def vector?(_), do: false

  def keyword([atom]) when is_atom(atom), do: atom
  def keyword([atom]), do: String.to_atom(atom)

  def cons([prepend, {:vector, list}]), do: [prepend | list]
  def cons([prepend, list]), do: [prepend | list]

  def concat(args) do
    Enum.map(args, &convert_vector/1)
      |> Enum.concat
  end
end
