namespace Mal;

function repl_environment(): Environment {
  return ns(new Environment());
}

function evaluate(Form $ast, Environment $environment): Form {
  $macro_result = define($ast, $environment) ??
    let($ast, $environment) ??
    do_all($ast, $environment) ??
    if_then_else($ast, $environment) ??
    fn($ast, $environment);
  if ($macro_result is nonnull) {
    return $macro_result;
  }
  $evaluated = eval_ast($ast, $environment);
  return function_call($ast, $evaluated) ?? $evaluated;
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

function function_call(Form $ast, Form $evaluated): ?Form {
  if ($ast is ListForm && C\count($ast->children) > 0) {
    $evaluated = $evaluated as ListForm;
    $function = C\firstx($evaluated->children);
    if (!$function is FunctionDefinition) {
      throw new EvalTypeException(FunctionDefinition::class, $function);
    }
    $callable = $function->function;
    return $callable($evaluated->children);
  }
  return null;
}

function define(Form $ast, Environment $environment): ?Form {
  $arguments = arguments_if_macro_call($ast, 'def!');
  if ($arguments is null) {
    return null;
  }
  $name = idx($arguments, 1);
  if ($name is null || !$name is Symbol) {
    throw new EvalException('Expected a symbol after `def!`');
  }
  $value = idx($arguments, 2);
  if ($value is null) {
    throw new EvalException('Expected a form as second argument to `def!`');
  }
  return $environment->set($name, evaluate($value, $environment));
}

function let(Form $ast, Environment $parent_environment): ?Form {
  $arguments = arguments_if_macro_call($ast, 'let*');
  if ($arguments is null) {
    return null;
  }
  $definitions = idx($arguments, 1);
  if (
    $definitions is null ||
    !($definitions is ListForm || $definitions is VectorForm)
  ) {
    throw new EvalException('Expected a list of definitions for `let*`');
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
      Str\format('Expected a value for a name `%s`', pr_str($name, true)),
    ),
  );
  $let_environment = new Environment($parent_environment);
  foreach ($definition_pairs as $key_value_pair) {
    list($key, $value) = $key_value_pair;
    $let_environment->set($key, evaluate($value, $let_environment));
  }
  $value = idx($arguments, 2);
  if ($value is null) {
    throw new EvalException('Expected a form as second argument to `let*`');
  }
  return evaluate($value, $let_environment);
}

function do_all(Form $ast, Environment $environment): ?Form {
  $arguments = arguments_if_macro_call($ast, 'do');
  if ($arguments is null) {
    return null;
  }
  if (C\count($arguments) < 2) {
    throw new EvalException('Expected at least one form as argument to `do`');
  }
  return C\lastx(evaluate_all(Vec\drop($arguments, 1), $environment));
}

function if_then_else(Form $ast, Environment $environment): ?Form {
  $arguments = arguments_if_macro_call($ast, 'if');
  if ($arguments is null) {
    return null;
  }
  $condition = idx($arguments, 1);
  if ($condition is null) {
    throw new EvalException('Expected a condition form for `if`');
  }
  $if_value = idx($arguments, 2);
  if ($if_value is null) {
    throw new EvalException('Expected an if branch form for `if`');
  }
  if (C\count($arguments) > 4) {
    throw new EvalException(
      'Unexpected form, only condition, if branch and else branch forms are '.
      'expected for `if`',
    );
  }
  $condition_result = evaluate($condition, $environment);
  if (
    $condition_result is GlobalNil ||
    $condition_result is BoolAtom && !$condition_result->value
  ) {
    $else_value = idx($arguments, 3);
    if ($else_value is null) {
      return new GlobalNil();
    } else {
      return evaluate($else_value, $environment);
    }
  }
  return evaluate($if_value, $environment);
}

function fn(Form $ast, Environment $closed_over_environment): ?Form {
  $arguments = arguments_if_macro_call($ast, 'fn*');
  if ($arguments is null) {
    return null;
  }
  $parameter_list = idx($arguments, 1);
  if ($parameter_list is null || !$parameter_list is ListLikeForm) {
    throw new EvalException('Expected a list of parameters for `fn`');
  }
  $parameter_names = Vec\map(
    $parameter_list->children,
    $parameter ==> {
      if (!$parameter is Symbol) {
        throw new EvalTypeException(Symbol::class, $parameter);
      }
      return $parameter;
    },
  );
  $body = idx($arguments, 2);
  if ($body is null) {
    throw new EvalException('Expected a body form for `fn`');
  }
  if (C\count($arguments) > 3) {
    throw new EvalException(
      'Unexpected form, only a parameter list and body form are '.
      'expected for `fn`',
    );
  }
  return new FunctionDefinition($function_arguments ==> {
    $fn_environment = new Environment($closed_over_environment);
    foreach ($parameter_names as $index => $parameter) {
      $value = idx($function_arguments, $index + 1);
      if ($value is null) {
        throw new EvalException(
          'Expected a value form for parameter '.$parameter->name,
        );
      }
      $fn_environment->set($parameter, $value);
    }
    return evaluate($body, $fn_environment);
  });
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
