namespace Mal;

type Environment = dict<string, Form>;

function repl_environment(): Environment {
  return dict[
    '+' => binary_numerical_function(($a, $b) ==> $a + $b),
    '-' => binary_numerical_function(($a, $b) ==> $a - $b),
    '*' => binary_numerical_function(($a, $b) ==> $a * $b),
    '/' => binary_numerical_function(($a, $b) ==> (int)($a / $b)),
  ];
}

function evaluate(Form $ast, Environment $environment): Form {
  $result = eval_ast($ast, $environment);
  if ($result is ListForm && C\count($result->children) > 0) {
    $function = C\firstx($result->children);
    if (!$function is FunctionDefinition) {
      throw new EvalTypeException(FunctionDefinition::class, $function);
    }
    $callable = $function->function;
    return $callable($result->children);
  }
  return $result;
}

function eval_ast(Form $ast, Environment $environment): Form {
  if ($ast is Symbol) {
    $resolved = idx($environment, $ast->name);
    if ($resolved is null) {
      throw new EvalException(
        'No definition found in scope for symbol `'.$ast->name.'`',
      );
    }
    return $resolved;
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
