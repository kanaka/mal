namespace Mal;


function ns(Environment $environment): Environment {
  $functions = vec[
    binary_numerical_function('+', ($a, $b) ==> $a + $b),
    binary_numerical_function('-', ($a, $b) ==> $a - $b),
    binary_numerical_function('*', ($a, $b) ==> $a * $b),
    binary_numerical_function('/', ($a, $b) ==> (int)($a / $b)),
    binary_comparison_function('<', ($a, $b) ==> $a < $b),
    binary_comparison_function('<=', ($a, $b) ==> $a <= $b),
    binary_comparison_function('>', ($a, $b) ==> $a > $b),
    binary_comparison_function('>=', ($a, $b) ==> $a >= $b),
    pr_str_function(),
    str_function(),
    prn_function(),
    println_function(),
    list_function(),
    is_list_function(),
    is_empty_function(),
    count_function(),
    equals_function(),
  ];
  foreach ($functions as $name_function_pair) {
    list($name, $function) = $name_function_pair;
    $environment->set($name, $function);
  }
  return $environment;
}

function binary_numerical_function(
  string $name,
  (function(int, int): int) $operation,
): (Symbol, FunctionDefinition) {
  return binary_function($name, $operation, $value ==> new Number($value));
}

function binary_comparison_function(
  string $name,
  (function(int, int): bool) $operation,
): (Symbol, FunctionDefinition) {
  return binary_function($name, $operation, $value ==> new BoolAtom($value));
}

function binary_function<T>(
  string $name,
  (function(int, int): T) $operation,
  (function(T): Form) $result_type,
): (Symbol, FunctionDefinition) {
  return named_function(
    $name,
    $arguments ==> {
      $a = $arguments[1];
      if (!$a is Number) {
        throw new EvalTypeException(Number::class, $a);
      }
      $b = $arguments[2];
      if (!$b is Number) {
        throw new EvalTypeException(Number::class, $b);
      }
      return $result_type($operation($a->value, $b->value));
    },
  );
}

function pr_str_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'pr-str',
    $arguments ==> new StringAtom(pr_str_arguments($arguments, true, ' ')),
  );
}

function str_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'str',
    $arguments ==> new StringAtom(pr_str_arguments($arguments, false, '')),
  );
}

function prn_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'prn',
    $arguments ==> {
      echo pr_str_arguments($arguments, true, ' ')."\n";
      return new GlobalNil();
    },
  );
}

function println_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'println',
    $arguments ==> {
      echo pr_str_arguments($arguments, false, ' ')."\n";
      return new GlobalNil();
    },
  );
}

function pr_str_arguments(
  vec<Form> $arguments,
  bool $print_readably,
  string $delimiter,
): string {
  return Vec\drop($arguments, 1)
    |> Vec\map($$, $argument ==> pr_str($argument, $print_readably))
    |> Str\join($$, $delimiter);
}

function list_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'list',
    $arguments ==> Vec\drop($arguments, 1)
      |> new ListForm($$),
  );
}

function is_list_function(): (Symbol, FunctionDefinition) {
  $name = 'list?';
  return named_function(
    $name,
    $arguments ==> {
      if (C\count($arguments) > 2) {
        throw new EvalException(
          "Unexpected form, expected only one argument to `$name`",
        );
      }
      $maybe_list = idx($arguments, 1);
      if ($maybe_list is null) {
        throw new EvalException("Expected one argument to `$name`");
      }
      return new BoolAtom($maybe_list is ListForm);
    },
  );
}

function is_empty_function(): (Symbol, FunctionDefinition) {
  $name = 'empty?';
  return named_function(
    $name,
    $arguments ==> {
      $list = idx($arguments, 1);
      if ($list is null) {
        throw new EvalException("Expected one argument to `$name`");
      }
      if (!$list is ListLikeForm) {
        throw new EvalTypeException(ListLikeForm::class, $list);
      }
      if (C\count($arguments) > 2) {
        throw new EvalException(
          "Unexpected form, expected only one list form argument to `$name`",
        );
      }
      return new BoolAtom(C\is_empty($list->children));
    },
  );
}

function count_function(): (Symbol, FunctionDefinition) {
  $name = 'count';
  return named_function(
    $name,
    $arguments ==> {
      $list = idx($arguments, 1);
      if ($list is nonnull && $list is GlobalNil) {
        return new Number(0);
      }
      if ($list is null) {
        throw new EvalException("Expected one argument to `$name`");
      }
      if (!$list is ListLikeForm) {
        throw new EvalTypeException(ListLikeForm::class, $list);
      }
      if (C\count($arguments) > 2) {
        throw new EvalException(
          "Unexpected form, expected only one list form argument to `$name`",
        );
      }
      return new Number(C\count($list->children));
    },
  );
}

function equals_function(): (Symbol, FunctionDefinition) {
  $name = '=';
  return named_function(
    $name,
    $arguments ==> {
      $a = idx($arguments, 1);
      $b = idx($arguments, 2);
      if ($a is null || $b is null) {
        throw new EvalException("Expected two arguments to `$name`");
      }
      if (C\count($arguments) > 3) {
        throw new EvalException(
          "Unexpected form, expected only one two arguments to `$name`",
        );
      }
      return new BoolAtom(equals_impl($a, $b));
    },
  );
}

function equals_impl(Form $a, Form $b): bool {
  if ($a is Number) {
    if ($b is Number) {
      return $a->value === $b->value;
    }
  } else if ($a is Keyword) {
    if ($b is Keyword) {
      return $a->name === $b->name;
    }
  } else if ($a is StringAtom) {
    if ($b is StringAtom) {
      return $a->value === $b->value;
    }
  } else if ($a is Symbol) {
    if ($b is Symbol) {
      return $a->name === $b->name;
    }
  } else if ($a is GlobalNil) {
    if ($b is GlobalNil) {
      return true;
    }
  } else if ($a is BoolAtom) {
    if ($b is BoolAtom) {
      return $a->value === $b->value;
    }
  } else if ($a is ListLikeForm) {
    if ($b is ListLikeForm) {
      return equals_children($a->children, $b->children);
    }
  } else if ($a is HashMapForm) {
    if ($b is HashMapForm) {
      return equals_children(
        join_pairs(children_pairs($a)),
        join_pairs(children_pairs($b)),
      );
    }
  } else if ($a is FunctionDefinition) {
    if ($b is FunctionDefinition) {
    }
  } else {
    throw new EvalException("Unsupported form type in =");
  }
  return false;
}

function equals_children(vec<Form> $children_a, vec<Form> $children_b): bool {
  if (C\count($children_a) !== C\count($children_b)) {
    return false;
  }
  return C\every(zip_map(
    $children_a,
    $children_b,
    ($a_child, $b_child) ==> equals_impl($a_child, $b_child),
  ));
}

function named_function(
  string $name,
  (function(vec<Form>): Form) $definition,
): (Symbol, FunctionDefinition) {
  return tuple(new Symbol($name), new FunctionDefinition($definition));
}

function zip_map<Ta, Tb, Tc>(
  vec<Ta> $list_a,
  vec<Tb> $list_b,
  (function(Ta, Tb): Tc) $map,
): vec<Tc> {
  $result = vec[];
  foreach ($list_a as $i => $a) {
    $result[] = $map($a, $list_b[$i]);
  }
  return $result;
}
