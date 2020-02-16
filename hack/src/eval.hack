namespace Mal;

function repl_environment(): Environment {
  return ns(new Environment());
}

type EvalResult = (bool, Form, Environment);

function evaluate(Form $ast, Environment $environment): Form {
  while (true) {
    $eval_result = apply($ast, $environment) ??
      define($ast, $environment) ??
      let($ast, $environment) ??
      do_all($ast, $environment) ??
      if_then_else($ast, $environment) ??
      fn($ast, $environment);
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
      throw new EvalTypeException(FunctionLike::class, $function);
    }
    return function_call_impl($function, $evaluated->children, $environment);
  }
  return null;
}

function apply(Form $ast, Environment $environment): ?EvalResult {
  $macro_name = 'apply';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $evaluated = Vec\concat(
    vec[$arguments[0]],
    evaluate_all(Vec\drop($arguments, 1), $environment),
  );

  $function = idx($evaluated, 1);
  if (!$function is FunctionLike) {
    throw new EvalTypedArgumentException(
      $macro_name,
      1,
      FunctionLike::class,
      $function,
    );
  }
  $last_index = C\count($evaluated) - 1;
  $list_argument = idx($evaluated, $last_index);
  if (!$list_argument is ListLikeForm) {
    throw new EvalTypedArgumentException(
      $macro_name,
      $last_index,
      ListLikeForm::class,
      $list_argument,
    );
  }
  $prepend_arguments = Vec\slice($evaluated, 1, C\count($evaluated) - 2);
  return function_call_impl(
    $function,
    Vec\concat($prepend_arguments, $list_argument->children),
    $environment,
  );
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
  $macro_name = 'def!';
  $arguments = arguments_if_macro_call($ast, $macro_name);
  if ($arguments is null) {
    return null;
  }
  $name = idx($arguments, 1);
  if (!$name is Symbol) {
    throw new EvalTypedArgumentException($macro_name, 1, Symbol::class, $name);
  }
  $value = idx($arguments, 2);
  if ($value is null) {
    throw new EvalUntypedArgumentException($macro_name, 2);
  }
  return eval_done(
    $environment->set($name, evaluate($value, $environment)),
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
    throw new EvalTypedArgumentException(
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
        throw new EvalTypeException(Symbol::class, $name);
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
    throw new EvalUntypedArgumentException($macro_name, 2);
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
    throw new EvalUntypedArgumentException($macro_name, 1, 'a condition form');
  }
  $if_value = idx($arguments, 2);
  if ($if_value is null) {
    throw new EvalUntypedArgumentException($macro_name, 2, 'an if branch form');
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
    throw new EvalTypedArgumentException(
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
        throw new EvalTypeException(Symbol::class, $parameter);
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
    throw new EvalUntypedArgumentException($macro_name, 2, 'a body form');
  }
  enforce_arity($macro_name, 2, $arguments);
  return eval_done(
    new FunctionWithTCODefinition(
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
