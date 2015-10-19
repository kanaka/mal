defmodule Mal.Reader do
  import Mal.Types

  def read_str(input) do
    case tokenize(input) do
      [] -> nil
      tokens -> tokens
        |> read_form
        |> elem(0)
    end
  end

  def tokenize(input) do
    regex = ~r/[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/
    Regex.scan(regex, input, capture: :all_but_first)
      |> List.flatten
      |> List.delete_at(-1) # Remove the last match, which is an empty string
      |> Enum.filter(fn token -> not String.starts_with?(token, ";") end)
  end

  defp read_form([next | rest] = tokens) do
    case next do
      "(" -> read_list(tokens)
      "[" -> read_vector(tokens)
      "{" -> read_hash_map(tokens)
      "'" -> create_quote("quote", rest)
      "`" -> create_quote("quasiquote", rest)
      "~" -> create_quote("unquote", rest)
      "~@" -> create_quote("splice-unquote", rest)
      "@" -> create_quote("deref", rest)
      "^" -> create_meta(rest)
      ")" -> throw({:error, "unexpected )"})
      "]" -> throw({:error, "unexpected ]"})
      "}" -> throw({:error, "unexpected }"})
      _ ->
        token = read_atom(next)
        {token, rest}
    end
  end

  defp create_meta(tokens) do
    {meta, meta_rest} = read_form(tokens)
    {token, rest_tokens} = read_form(meta_rest)
    new_token = list([{:symbol, "with-meta"}, token, meta])
    {new_token, rest_tokens}
  end

  defp create_quote(quote_type, tokens) do
    {token, rest_tokens} = read_form(tokens)
    new_token = list([{:symbol, quote_type}, token])
    {new_token, rest_tokens}
  end

  defp read_list([_ | tokens]) do
    {ast, rest} = do_read_sequence(tokens, [], "(", ")")
    {list(ast), rest}
  end

  defp read_vector([_ | tokens]) do
    {ast, rest} = do_read_sequence(tokens, [], "[", "]")
    {vector(ast), rest}
  end

  defp read_hash_map([_ | tokens]) do
    {map, rest} = do_read_sequence(tokens, [], "{", "}")
    {hash_map(map), rest}
  end

  defp do_read_sequence([], _acc, _start_sep, end_sep), do: throw({:error, "expected #{end_sep}, got EOF"})
  defp do_read_sequence([head | tail] = tokens, acc, start_sep, end_sep) do
    cond do
      String.starts_with?(head, end_sep) ->
        {Enum.reverse(acc), tail}
      true ->
        {token, rest} = read_form(tokens)
        do_read_sequence(rest, [token | acc], start_sep, end_sep)
    end
  end

  defp read_atom("nil"), do: nil
  defp read_atom("true"), do: true
  defp read_atom("false"), do: false
  defp read_atom(":" <> rest), do: String.to_atom(rest)
  defp read_atom(token) do
    cond do
      String.starts_with?(token, "\"") and String.ends_with?(token, "\"") ->
        token
          |> String.slice(1..-2)
          |> String.replace("\\\"", "\"")

      integer?(token) ->
        Integer.parse(token)
          |> elem(0)

      true -> {:symbol, token}
    end
  end
end
