namespace Mal;

function repl_environment(): Environment {
  return new Environment(
    null,
    dict[
      '+' => binary_numerical_function(($a, $b) ==> $a + $b),
      '-' => binary_numerical_function(($a, $b) ==> $a - $b),
      '*' => binary_numerical_function(($a, $b) ==> $a * $b),
      '/' => binary_numerical_function(($a, $b) ==> (int)($a / $b)),
    ],
  );
}

function evaluate(Form $ast, Environment $environment): Form {
  $macro_result = define($ast, $environment) ?? let($ast, $environment);
  if ($macro_result is nonnull) {
    return $macro_result;
  }
  $evaluated = eval_ast($ast, $environment);
  return function_call($evaluated) ?? $evaluated;
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

function function_call(Form $evaluated): ?Form {
  if ($evaluated is ListForm && C\count($evaluated->children) > 0) {
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

function arguments_if_macro_call(Form $ast, string $macro_name): ?vec<Form> {
  if ($ast is ListForm && C\count($ast->children) > 0) {
    $macro = C\firstx($ast->children);
    if ($macro is Symbol && $macro->name === $macro_name) {
      return $ast->children;
    }
  }
  return null;
}

function binary_numerical_function(
  (function(int, int): int) $operation,
): FunctionDefinition {
  return new FunctionDefinition(
    $arguments ==> {
      $a = $arguments[1];
      if (!$a is Number) {
        throw new EvalTypeException(Number::class, $a);
      }
      $b = $arguments[2];
      if (!$b is Number) {
        throw new EvalTypeException(Number::class, $b);
      }
      return new Number($operation($a->value, $b->value));
    },
  );
}
