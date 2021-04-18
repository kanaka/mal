namespace Mal;

function repl_environment(): Environment {
  return ns(new Environment());
}

type EvalResult = (bool, Form, Environment);

function evaluate(Form $ast, Environment $environment): Form {
  while (true) {
    $eval_result = define($ast, $environment) ??
      define_macro($ast, $environment) ??
      let($ast, $environment) ??
      do_all($ast, $environment) ??
      if_then_else($ast, $environment) ??
      fn($ast, $environment) ??
      quote($ast, $environment) ??
      quasiquote($ast, $environment) ??
      try_catch($ast, $environment) ??
      macroexpand($ast, $environment);
    $eval_result = $eval_result ?? expand_macro($ast, $environment);
    if ($eval_result is null) {
      $evaluated = eval_ast($ast, $environment);
      $eval_result = function_call($ast, $evaluated, $environment) ??
        eval_done($evaluated, $environment);
    }
    list($should_continue, $ast, $environment) = $eval_result;
    if (!$should_continue) {
      return $ast;
    }
  }
}

function eval_ast(Form $ast, Environment $environment): Form {
  if ($ast is Symbol) {
    return $environment->get($ast);
  } else if ($ast is ListForm) {
    return new ListForm(evaluate_all($ast->children, $environment));
  } else if ($ast is VectorForm) {
    return new VectorForm(evaluate_all($ast->children, $environment));
  } else if ($ast is HashMapForm) {
    return pairs_to_map(
      Vec\map(
        children_pairs($ast),
        $key_value_pair ==> {
          list($key, $value) = $key_value_pair;
          return tuple($key, evaluate($value, $environment));
        },
      ),
    );
  } else {
    return $ast;
  }
}

function evaluate_all(vec<Form> $forms, Environment $environment): vec<Form> {
  return Vec\map($forms, $child ==> evaluate($child, $environment));
}

function function_call(
  Form $ast,
  Form $evaluated,
  Environment $environment,
): ?EvalResult {
  if ($ast is ListForm && C\count($ast->children) > 0) {
    $evaluated = $evaluated as ListForm;
    $function = C\firstx($evaluated->children);
    if (!$function is FunctionLike) {
      throw new TypeException('(', FunctionLike::class, $function);
    }
    return function_call_impl($function, $evaluated->children, $environment);
  }
  return null;
}

function expand_macro(Form $ast, Environment $environment): ?EvalResult {
  if ($ast is ListForm) {
    $maybe_macro_name = C\first($ast->children);
    if ($maybe_macro_name is Symbol) {
      $maybe_macro = $environment->get($maybe_macro_name);
      if ($maybe_macro is FunctionWithTCODefinition && $maybe_macro->is_macro) {
        $callable = $maybe_macro->unoptimized->function;
        return eval_tco($callable($ast->children), $environment);
      }
    }
  }
  return null;
}

function macroexpand(Form $ast, Environment $environment): ?EvalResult {
  $macro_name = 'macroexpand';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $macro_call = idx($arguments, 1);
  if ($macro_call is null) {
    throw new MissingArgumentException($macro_name, 1);
  }
  $result = expand_macro($macro_call, $environment);
  if ($result is null) {
    throw new TypedArgumentException(
      $macro_name,
      1,
      ListForm::class,
      $macro_call,
      'a macro call',
    );
  }
  list($_, $expanded, $environment) = $result;
  return eval_done($expanded, $environment);
}

function function_call_impl(
  FunctionLike $function,
  vec<Form> $arguments,
  Environment $environment,
): ?EvalResult {
  if ($function is FunctionDefinition) {
    $callable = $function->function;
    return eval_done($callable($arguments), $environment);
  }
  if ($function is FunctionWithTCODefinition) {
    return eval_tco(
      $function->body,
      function_call_bind(
        $function->parameters,
        $arguments,
        $function->closed_over_environment,
      ),
    );
  }
  invariant(false, 'Unsupported subtype of FunctionLike');
}

function define(Form $ast, Environment $environment): ?EvalResult {
  return define_impl('def!', $ast, $environment, $value ==> $value);
}

function define_macro(Form $ast, Environment $environment): ?EvalResult {
  $macro_name = 'defmacro!';
  return define_impl(
    $macro_name,
    $ast,
    $environment,
    $function ==> {
      if (!$function is FunctionWithTCODefinition) {
        throw new TypedArgumentException(
          $macro_name,
          2,
          FunctionLike::class,
          $function,
        );
      }
      return new FunctionWithTCODefinition(
        true,
        $function->body,
        $function->parameters,
        $function->closed_over_environment,
        $function->unoptimized,
      );
    },
  );
}

function define_impl(
  string $macro_name,
  Form $ast,
  Environment $environment,
  (function(Form): Form) $tranform_result,
): ?EvalResult {
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $name = idx($arguments, 1);
  if (!$name is Symbol) {
    throw new TypedArgumentException($macro_name, 1, Symbol::class, $name);
  }
  $value = idx($arguments, 2);
  if ($value is null) {
    throw new MissingArgumentException($macro_name, 2);
  }
  return eval_done(
    $environment->set($name, $tranform_result(evaluate($value, $environment))),
    $environment,
  );
}

function let(Form $ast, Environment $parent_environment): ?EvalResult {
  $macro_name = 'let*';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $definitions = idx($arguments, 1);
  if (!($definitions is ListLikeForm)) {
    throw new TypedArgumentException(
      $macro_name,
      1,
      ListLikeForm::class,
      $definitions,
      'a list of definitions',
    );
  }
  $definition_pairs = read_pairs(
    $definitions->children,
    $name ==> {
      if (!$name is Symbol) {
        throw new TypeException($macro_name, Symbol::class, $name, 'a name');
      }
      return $name;
    },
    $name ==> new EvalException(
      Str\format(
        'Expected a value for a `%s` binding of `%s`',
        $macro_name,
        pr_str($name, true),
      ),
    ),
  );
  $let_environment = new Environment($parent_environment);
  foreach ($definition_pairs as $name_value_pair) {
    list($name, $value) = $name_value_pair;
    $let_environment->set($name, evaluate($value, $let_environment));
  }
  $value = idx($arguments, 2);
  if ($value is null) {
    throw new MissingArgumentException($macro_name, 2);
  }
  return eval_tco($value, $let_environment);
}

function do_all(Form $ast, Environment $environment): ?EvalResult {
  $arguments = arguments_if_macro_call($ast, 'do');
  if ($arguments is null) {
    return null;
  }
  if (C\count($arguments) < 2) {
    throw new EvalException('Expected at least one form as argument to `do`');
  }
  evaluate_all(Vec\slice($arguments, 1, C\count($arguments) - 2), $environment);
  return eval_tco(C\lastx($arguments), $environment);
}

function if_then_else(Form $ast, Environment $environment): ?EvalResult {
  $macro_name = 'if';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $condition = idx($arguments, 1);
  if ($condition is null) {
    throw new MissingArgumentException($macro_name, 1, 'a condition form');
  }
  $if_value = idx($arguments, 2);
  if ($if_value is null) {
    throw new MissingArgumentException($macro_name, 2, 'an if branch form');
  }
  enforce_arity($macro_name, 3, $arguments);
  $condition_result = evaluate($condition, $environment);
  if (
    $condition_result is GlobalNil ||
    $condition_result is BoolAtom && !$condition_result->value
  ) {
    $else_value = idx($arguments, 3);
    if ($else_value is null) {
      return eval_done(new GlobalNil(), $environment);
    } else {
      return eval_tco($else_value, $environment);
    }
  }
  return eval_tco($if_value, $environment);
}

function fn(Form $ast, Environment $closed_over_environment): ?EvalResult {
  $macro_name = 'fn*';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $parameter_list = idx($arguments, 1);
  if (!$parameter_list is ListLikeForm) {
    throw new TypedArgumentException(
      $macro_name,
      1,
      ListLikeForm::class,
      $parameter_list,
      'a list of parameters',
    );
  }
  $parameter_names = Vec\map_with_key(
    $parameter_list->children,
    ($index, $parameter) ==> {
      if (!$parameter is Symbol) {
        throw new TypeException($macro_name, Symbol::class, $parameter);
      }
      if (
        $parameter->name === '&' &&
        C\count($parameter_list->children) > $index + 2
      ) {
        throw new EvalException(
          'Expected only one variadic parameter after `&` for `fn`',
        );
      }
      return $parameter;
    },
  );
  $body = idx($arguments, 2);
  if ($body is null) {
    throw new MissingArgumentException($macro_name, 2, 'a body form');
  }
  enforce_arity($macro_name, 2, $arguments);
  return eval_done(
    new FunctionWithTCODefinition(
      false,
      $body,
      $parameter_names,
      $closed_over_environment,
      new FunctionDefinition($function_arguments ==> {
        return evaluate($body, function_call_bind(
          $parameter_names,
          $function_arguments,
          $closed_over_environment,
        ));
      }),
    ),
    $closed_over_environment,
  );
}

function function_call_bind(
  vec<Symbol> $parameters,
  vec<Form> $arguments,
  Environment $closed_over_environment,
): Environment {
  $fn_environment = new Environment($closed_over_environment);
  foreach ($parameters as $index => $parameter) {
    if ($parameter->name === '&') {
      $fn_environment->set(
        $parameters[$index + 1],
        new ListForm(Vec\drop($arguments, $index + 1)),
      );
      break;
    }
    $value = idx($arguments, $index + 1);
    if ($value is null) {
      throw new EvalException(
        'In function call expected a value form for parameter `'.
        $parameter->name.
        '`',
      );
    }
    $fn_environment->set($parameter, $value);
  }
  return $fn_environment;
}

function quote(Form $ast, Environment $environment): ?EvalResult {
  $macro_name = 'quote';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $value = idx($arguments, 1);
  if ($value is null) {
    throw new MissingArgumentException($macro_name, 1);
  }
  return eval_done($value, $environment);
}

function quasiquote(Form $ast, Environment $environment): ?EvalResult {
  $quasiquote_name = 'quasiquote';
  $unquote_name = 'unquote';
  $splice_unquote_name = 'splice-unquote';
  $arguments = arguments_if_macro_call($ast, $quasiquote_name);
  if ($arguments is null) {
    return null;
  }
  $value = idx($arguments, 1);
  if ($value is null) {
    throw new MissingArgumentException($quasiquote_name, 1);
  }
  if (!$value is ListLikeForm || C\is_empty($value->children)) {
    return eval_tco(new_function_call('quote', vec[$value]), $environment);
  } else {
    $first_item = C\firstx($value->children);
    if ($first_item is Symbol && $first_item->name === $unquote_name) {
      $unqouted = idx($value->children, 1);
      if ($unqouted is null) {
        throw new MissingArgumentException($unquote_name, 1);
      }
      return eval_tco($unqouted, $environment);
    } else {
      $rest_items = Vec\drop($value->children, 1);
      if ($first_item is ListForm && !C\is_empty($first_item->children)) {
        $nested_first_item = C\firstx($first_item->children);
        if (
          $nested_first_item is Symbol &&
          $nested_first_item->name === $splice_unquote_name
        ) {
          $splice_unqouted = idx($first_item->children, 1);
          if ($splice_unqouted is null) {
            throw new MissingArgumentException($splice_unquote_name, 1);
          }
          return eval_tco(
            new_function_call(
              'concat',
              vec[
                $splice_unqouted,
                new_function_call(
                  $quasiquote_name,
                  vec[new ListForm($rest_items)],
                ),
              ],
            ),
            $environment,
          );
        }
      }
      return eval_tco(
        new_function_call(
          'cons',
          vec[
            new_function_call($quasiquote_name, vec[$first_item]),
            new_function_call($quasiquote_name, vec[new ListForm($rest_items)]),
          ],
        ),
        $environment,
      );
    }
  }
}

function try_catch(Form $ast, Environment $environment): ?EvalResult {
  $try_name = 'try*';
  $catch_name = 'catch*';
  $arguments = arguments_if_macro_call($ast, $try_name);
  if ($arguments is null) {
    return null;
  }
  $tried = idx($arguments, 1);
  if ($tried is null) {
    throw new MissingArgumentException($try_name, 1);
  }
  $catch = idx($arguments, 2);
  if ($catch is null) {
    return eval_tco($tried, $environment);
  }
  if (!$catch is ListForm) {
    throw new TypedArgumentException(
      $try_name,
      2,
      ListForm::class,
      $catch,
      'a catch form',
    );
  }
  $catch_symbol = idx($catch->children, 0);
  if (!($catch_symbol is Symbol && $catch_symbol->name === $catch_name)) {
    throw new TypedArgumentException(
      $try_name,
      2,
      ListForm::class,
      $catch,
      "a call to `$catch_name`",
    );
  }
  $exception_name = idx($catch->children, 1);
  if (!$exception_name is Symbol) {
    throw new TypedArgumentException(
      $catch_name,
      1,
      Symbol::class,
      $catch,
      'catch form',
    );
  }
  $result_on_exception = idx($catch->children, 2);
  if ($result_on_exception is null) {
    throw new MissingArgumentException($catch_name, 2);
  }
  try {
    return eval_done(evaluate($tried, $environment), $environment);
  } catch (\Throwable $e) {
    $thrown = $e is ThrownException
      ? $e->thrown
      : new StringAtom($e->getMessage());
    $catch_environment = new Environment(
      $environment,
      dict[$exception_name->name => $thrown],
    );
    return eval_tco($result_on_exception, $catch_environment);
  }
}

function new_function_call(string $name, vec<Form> $arguments): ListForm {
  return new ListForm(Vec\concat(vec[new Symbol($name)], $arguments));
}

function arguments_if_macro_call(Form $ast, string $macro_name): ?vec<Form> {
  if ($ast is ListForm && C\count($ast->children) > 0) {
    $macro = C\firstx($ast->children);
    if ($macro is Symbol && $macro->name === $macro_name) {
      return $ast->children;
    }
  }
  return null;
}

function eval_done(
  Form $ast,
  Environment $environment,
): (bool, Form, Environment) {
  return tuple(false, $ast, $environment);
}

function eval_tco(
  Form $ast,
  Environment $environment,
): (bool, Form, Environment) {
  return tuple(true, $ast, $environment);
}
