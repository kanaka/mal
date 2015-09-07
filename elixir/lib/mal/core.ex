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
      "assoc" => &assoc/1,
      "dissoc" => &dissoc/1,
      "get" => &get/1,
      "hash-map" => &Mal.Types.hash_map/1,
      "readline" => fn [prompt] -> readline(prompt) end,
      "sequential?" => fn arg -> vector?(arg) or list?(arg) end,
      "vector" => fn list -> {:vector, list} end,
      "keyword?" => fn [type] -> is_atom(type) end,
      "map?" => fn [type] -> is_map(type) end,
      "nil?" => fn [type] -> type == nil end,
      "true?" => fn [type] -> type == true end,
      "false?" => fn [type] -> type == false end,
      "symbol" => fn [name] -> {:symbol, name} end,
      "list" => fn args -> args end,
      "read-string" => fn [input] -> Mal.Reader.read_str(input) end,
      "throw" => fn [arg] -> throw({:error, arg}) end,
      "contains?" => fn [map, key] -> Map.has_key?(map, key) end,
      "keys" => fn [map] -> Map.keys(map) end,
      "vals" => fn [map] -> Map.values(map) end
    }
  end

  def readline(prompt) do
    IO.write(:stdio, prompt)
    IO.read(:stdio, :line)
      |> String.strip(?\n)
  end

  defp convert_vector({:vector, list}), do: list
  defp convert_vector(other), do: other

  defp equal([a, b]) do
    convert_vector(a) == convert_vector(b)
  end

  defp list?([arg]) when is_list(arg), do: true
  defp list?([_arg]), do: false

  defp empty?([[]]), do: true
  defp empty?([{:vector, []}]), do: true
  defp empty?(_), do: false

  defp count([arg]) when is_list(arg), do: length(arg)
  defp count([{:vector, arg}]), do: length(arg)
  defp count(_), do: 0

  defp pr_str(args) do
    args
      |> Enum.map(&Mal.Printer.print_str/1)
      |> Enum.join(" ")
  end

  defp str(args) do
    args
      |> Enum.map(&(Mal.Printer.print_str(&1, false)))
      |> Enum.join("")
  end

  defp prn(args) do
    args
      |> pr_str
      |> IO.puts
    nil
  end

  defp println(args) do
    args
      |> Enum.map(&(Mal.Printer.print_str(&1, false)))
      |> Enum.join(" ")
      |> IO.puts
    nil
  end

  defp slurp([file_name]) do
    case File.read(file_name) do
      {:ok, content} -> content
      {:error, :enoent} -> throw({:error, "can't find file #{file_name}"})
      {:error, :eisdir} -> throw({:error, "can't read directory #{file_name}"})
      {:error, :eaccess} -> throw({:error, "missing permissions #{file_name}"})
      {:error, reason} -> throw({:error, "can't read file #{file_name}, #{reason}"})
    end
  end

  defp nth([list, index]) do
    case Enum.at(convert_vector(list), index, :error) do
      :error -> throw({:error, "index out of bounds"})
      any -> any
    end
  end

  defp first([{:vector, [head | tail]}]), do: head
  defp first([[head | tail]]), do: head
  defp first(_), do: nil

  defp rest([{:vector, list}]), do: do_rest(list)
  defp rest([list]), do: do_rest(list)

  defp do_rest([head | tail]), do: tail
  defp do_rest([]), do: []

  defp map([{_function_type, function}, list]), do: do_map(function, list)
  defp map([function, list]), do: do_map(function, list)

  defp do_map(function, list) do
    convert_vector(list)
      |> Enum.map(fn arg -> function.([arg]) end)
  end

  defp apply([{_function_type, function} | tail]), do: do_apply(function, tail)
  defp apply([function | tail]), do: do_apply(function, tail)

  defp do_apply(function, tail) do
    [list | reversed_args] = Enum.reverse(tail)
    args = Enum.reverse(reversed_args)
    func_args = Enum.concat(args, convert_vector(list))
    function.(func_args)
  end

  defp symbol?([{:symbol, _}]), do: true
  defp symbol?(_), do: false

  defp vector?([{:vector, _}]), do: true
  defp vector?(_), do: false

  defp keyword([atom]) when is_atom(atom), do: atom
  defp keyword([atom]), do: String.to_atom(atom)

  defp cons([prepend, {:vector, list}]), do: [prepend | list]
  defp cons([prepend, list]), do: [prepend | list]

  defp concat(args) do
    Enum.map(args, &convert_vector/1)
      |> Enum.concat
  end

  defp assoc([hash_map | pairs]) do
    Map.merge(hash_map, Mal.Types.hash_map(pairs))
  end

  defp dissoc([hash_map | keys]) do
    Map.drop(hash_map, keys)
  end

  defp get([map, key]) when is_map(map), do: Map.get(map, key, nil)
  defp get([_map, _key]), do: nil
end
