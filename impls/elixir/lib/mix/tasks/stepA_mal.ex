defmodule Mix.Tasks.StepAMal do
  import Mal.Types
  alias Mal.Function

  # for escript execution
  def main(args) do
    run(args)
  end

  def run(args) do
    env = Mal.Env.new()
    Mal.Env.merge(env, Mal.Core.namespace)
    bootstrap(args, env)
    loop(env)
  end

  defp load_file(file_name, env) do
    read_eval_print("""
      (load-file "#{file_name}")
      """, env)
    exit(:normal)
  end

  defp bootstrap(args, env) do
    # *host-language*
    read_eval_print("(def! *host-language* \"Elixir\")", env)

    # not:
    read_eval_print("""
      (def! not
        (fn* (a) (if a false true)))
      """, env)

    # load-file:
    read_eval_print("""
      (def! load-file
        (fn* (f)
          (eval (read-string (str "(do " (slurp f) "\nnil)")))))
      """, env)

    # cond
    read_eval_print("""
      (defmacro! cond
        (fn* (& xs)
          (if (> (count xs) 0)
            (list 'if (first xs)
              (if (> (count xs) 1)
                (nth xs 1)
                (throw \"odd number of forms to cond\"))
              (cons 'cond (rest (rest xs)))))))"
      """, env)

    Mal.Env.set(env, "eval", %Function{value: fn [ast] ->
      eval(ast, env)
    end})

    case args do
      [file_name | rest] ->
        Mal.Env.set(env, "*ARGV*", list(rest))
        load_file(file_name, env)

      [] ->
        Mal.Env.set(env, "*ARGV*", list([]))
        read_eval_print("(println (str \"Mal [\" *host-language* \"]\"))", env)
    end
  end

  defp loop(env) do
    IO.write(:stdio, "user> ")
    IO.read(:stdio, :line)
      |> read_eval_print(env)
      |> IO.puts

    loop(env)
  end

  defp eval_ast({:list, ast, meta}, env) when is_list(ast) do
    {:list, Enum.map(ast, fn elem -> eval(elem, env) end), meta}
  end

  defp eval_ast({:map, ast, meta}, env) do
    map = for {key, value} <- ast, into: %{} do
      {eval(key, env), eval(value, env)}
    end

    {:map, map, meta}
  end

  defp eval_ast({:vector, ast, meta}, env) do
    {:vector, Enum.map(ast, fn elem -> eval(elem, env) end), meta}
  end

  defp eval_ast({:symbol, symbol}, env) do
    case Mal.Env.get(env, symbol) do
      {:ok, value} -> value
      :not_found -> throw({:error, "'#{symbol}' not found"})
    end
  end

  defp eval_ast(ast, _env), do: ast

  defp read(input) do
    Mal.Reader.read_str(input)
  end

  defp eval_bindings([], env), do: env
  defp eval_bindings([{:symbol, key}, binding | tail], env) do
    evaluated = eval(binding, env)
    Mal.Env.set(env, key, evaluated)
    eval_bindings(tail, env)
  end
  defp eval_bindings(_bindings, _env), do: throw({:error, "Unbalanced let* bindings"})

  defp quasiquote({:list, [{:symbol, "unquote"}, arg], _}), do: arg
  defp quasiquote({:list, [{:symbol, "unquote"}|   _], _}), do: throw({:error, "unquote: arg count"})
  defp quasiquote({:list,   xs, _}), do:                         qq_foldr(xs)
  defp quasiquote({:vector, xs, _}), do: list([{:symbol, "vec"}, qq_foldr(xs)])
  defp quasiquote({:symbol, sym}),    do: list([{:symbol, "quote"}, {:symbol, sym}])
  defp quasiquote({:map, ast, meta}), do: list([{:symbol, "quote"}, {:map, ast, meta}])
  defp quasiquote(ast), do: ast

  defp qq_foldr([]),     do: list([])
  defp qq_foldr([x|xs]), do: qq_loop(x, qq_foldr xs)

  defp qq_loop({:list, [{:symbol, "splice-unquote"}, arg], _}, acc), do: list([{:symbol, "concat"}, arg, acc])
  defp qq_loop({:list, [{:symbol, "splice-unquote"}|   _], _},   _), do: throw({:error, "splice-unquote: arg count"})
  defp qq_loop(elt, acc), do: list([{:symbol, "cons"}, quasiquote(elt), acc])

  defp macro_call?({:list, [{:symbol, key} | _tail], _}, env) do
    case Mal.Env.get(env, key) do
      {:ok, %Function{macro: true}} -> true
      _ -> false
    end
  end
  defp macro_call?(_ast, _env), do: false

  defp do_macro_call({:list, [{:symbol, key} | tail], _}, env) do
    {:ok, %Function{value: macro, macro: true}} = Mal.Env.get(env, key)
    macro.(tail)
      |> macroexpand(env)
  end

  defp macroexpand(ast, env) do
    if macro_call?(ast, env) do
      do_macro_call(ast, env)
    else
      ast
    end
  end

  defp eval({:list, [], _} = empty_ast, _env), do: empty_ast
  defp eval({:list, _list, _meta} = ast, env) do
    case macroexpand(ast, env) do
      {:list, list, meta} -> eval_list(list, env, meta)
      result -> eval_ast(result, env)
    end
  end
  defp eval(ast, env), do: eval_ast(ast, env)

  defp eval_list([{:symbol, "macroexpand"}, ast], env, _), do: macroexpand(ast, env)

  defp eval_list([{:symbol, "if"}, condition, if_true | if_false], env, _) do
    result = eval(condition, env)
    if result == nil or result == false do
      case if_false do
        [] -> nil
        [body] -> eval(body, env)
      end
    else
      eval(if_true, env)
    end
  end

  defp eval_list([{:symbol, "do"} | ast], env, _) do
    ast
      |> List.delete_at(-1)
      |> list
      |> eval_ast(env)
    eval(List.last(ast), env)
  end

  defp eval_list([{:symbol, "def!"}, {:symbol, key}, value], env, _) do
    evaluated = eval(value, env)
    Mal.Env.set(env, key, evaluated)
    evaluated
  end

  defp eval_list([{:symbol, "defmacro!"}, {:symbol, key}, function], env, _) do
    macro = %{eval(function, env) | macro: true}
    Mal.Env.set(env, key, macro)
    macro
  end

  defp eval_list([{:symbol, "let*"}, {list_type, bindings, _}, body], env, _)
  when list_type == :list or list_type == :vector do
    let_env = Mal.Env.new(env)
    eval_bindings(bindings, let_env)
    eval(body, let_env)
  end

  defp eval_list([{:symbol, "fn*"}, {list_type, params, _}, body], env, _)
  when list_type == :list or list_type == :vector do
    param_symbols = for {:symbol, symbol} <- params, do: symbol

    closure = fn args ->
      inner = Mal.Env.new(env, param_symbols, args)
      eval(body, inner)
    end

    %Function{value: closure}
  end

  defp eval_list([{:symbol, "quote"}, arg], _env, _), do: arg

  defp eval_list([{:symbol, "quasiquoteexpand"}, ast], _, _) do
    quasiquote(ast)
  end

  defp eval_list([{:symbol, "quasiquote"}, ast], env, _) do
    ast |> quasiquote
      |> eval(env)
  end

  # (try* A (catch* B C))
  defp eval_list([{:symbol, "try*"}, try_form, {:list, catch_list, _meta}], env, _) do
    eval_try(try_form, catch_list, env)
  end
  defp eval_list([{:symbol, "try*"}, try_form], env, _) do
    eval(try_form, env)
  end
  defp eval_list([{:symbol, "try*"}, _try_form, _], _env, _) do
    throw({:error, "try* requires a list as the second parameter"})
  end

  defp eval_list(ast, env, meta) do
    {:list, [func | args], _} = eval_ast({:list, ast, meta}, env)
    func.value.(args)
  end

  defp eval_try(try_form,
  [{:symbol, "catch*"}, {:symbol, exception}, catch_form], env) do
    try do
      eval(try_form, env)
    catch
      {:error, message}->
        catch_env = Mal.Env.new(env)
        Mal.Env.set(catch_env, exception, {:exception, message})
        eval(catch_form, catch_env)
    end
  end
  defp eval_try(_try_form, _catch_list, _env) do
    throw({:error, "catch* requires two arguments"})
  end

  defp print(value) do
    Mal.Printer.print_str(value)
  end

  defp read_eval_print(:eof, _env), do: exit(:normal)
  defp read_eval_print(line, env) do
    read(line)
      |> eval(env)
      |> print
  catch
    {:error, exception} ->
      IO.puts("Error: #{Mal.Printer.print_str(exception)}")
  end
end
