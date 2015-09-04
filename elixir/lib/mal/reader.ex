# TODO: def -> defp for everything but read_str
defmodule Mal.Reader do
  import Mal.Types

  def read_str(input) do
    tokenize(input)
      |> read_form
      |> elem(0)
  end

  def tokenize(input) do
    regex = ~r/[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/
    Regex.scan(regex, input, capture: :all_but_first)
      |> List.flatten
      |> List.delete_at(-1) # Remove the last match, which is an empty string
      |> Enum.filter(fn token -> not String.starts_with?(token, ";") end)
  end

  def read_form([next | rest] = tokens) do
    case next do
      "(" <> _ ->
        read_list(tokens)
      "'" -> create_quote('quote', rest)
      "`" -> create_quote('quasiquote', rest)
      "~" -> create_quote('unquote', rest)
      "~@" -> create_quote('splice-unquote', rest)
      "@" -> create_quote('deref', rest)
      "^" -> create_meta(rest)
      ")" -> throw({:error, "unexpected )"})
      _ ->
        token = read_atom(next)
        {token, rest}
    end
  end

  defp create_meta(tokens) do
    {meta, meta_rest} = read_form(tokens)
    {token, rest_tokens} = read_form(meta_rest)
    new_token = [{:symbol, 'with-meta'}, token, meta]
    {new_token, rest_tokens}
  end

  defp create_quote(quote_type, tokens) do
    {token, rest_tokens} = read_form(tokens)
    new_token = [{:symbol, quote_type}, token]
    {new_token, rest_tokens}
  end

  def read_list([_ | tokens]), do: do_read_list(tokens, [])

  defp do_read_list([], _acc), do: throw({:error, "expected ')', got EOF"})
  defp do_read_list([head | tail] = tokens, acc) do
    case head do
      ")" <> _ -> {Enum.reverse(acc), tail}
      _  ->
        {token, rest} = read_form(tokens)
        do_read_list(rest, [token | acc])
    end
  end

  def read_atom("nil"), do: nil
  def read_atom("true"), do: true
  def read_atom("false"), do: false
  def read_atom(":" <> rest), do: String.to_atom(rest)
  def read_atom(token) do
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
