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
    read_string_function(),
    slurp(),
    eval_function($environment),
    atom_function(),
    is_atom_function(),
    deref_function(),
    reset_function(),
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
      $a = idx($arguments, 1);
      if (!$a is Number) {
        throw new EvalTypedArgumentException($name, 1, Number::class, $a);
      }
      $b = idx($arguments, 2);
      if (!$b is Number) {
        throw new EvalTypedArgumentException($name, 2, Number::class, $b);
      }
      enforce_arity($name, 2, $arguments);
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
      $maybe_list = idx($arguments, 1);
      if ($maybe_list is null) {
        throw new EvalException("Expected one argument to `$name`");
      }
      enforce_arity($name, 1, $arguments);
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
      if (!$list is ListLikeForm) {
        throw new EvalTypedArgumentException(
          $name,
          1,
          ListLikeForm::class,
          $list,
        );
      }
      enforce_arity($name, 1, $arguments);
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
      if (!$list is ListLikeForm) {
        throw new EvalTypedArgumentException(
          $name,
          1,
          ListLikeForm::class,
          $list,
        );
      }
      enforce_arity($name, 1, $arguments);
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
      enforce_arity($name, 2, $arguments);
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

function read_string_function(): (Symbol, FunctionDefinition) {
  $name = 'read-string';
  return named_function(
    $name,
    $arguments ==> {
      $string = idx($arguments, 1);
      if (!$string is StringAtom) {
        throw new EvalTypedArgumentException(
          $name,
          1,
          StringAtom::class,
          $string,
        );
      }
      return read_str($string->value);
    },
  );
}

function slurp(): (Symbol, FunctionDefinition) {
  $name = 'slurp';
  return named_function(
    $name,
    $arguments ==> {
      $string = idx($arguments, 1);
      if (!$string is StringAtom) {
        throw new EvalTypedArgumentException(
          $name,
          1,
          StringAtom::class,
          $string,
        );
      }
      try {
        return \HH\Asio\join(
          async {
            await using $file_handle = File\open_read_only($string->value);
            return new StringAtom(await $file_handle->readAsync());
          },
        );
      } catch (OS\NotFoundException $e) {
        throw new EvalException('File `'.$string->value.'` not found');
      }
    },
  );
}

function eval_function(Environment $environment): (Symbol, FunctionDefinition) {
  $name = 'eval';
  return named_function(
    $name,
    $arguments ==> {
      $ast = idx($arguments, 1);
      if ($ast is null) {
        throw new EvalUntypedArgumentException($name, 1);
      }
      return evaluate($ast, $environment);
    },
  );
}

function atom_function(): (Symbol, FunctionDefinition) {
  $name = 'atom';
  return named_function(
    $name,
    $arguments ==> {
      $value = idx($arguments, 1);
      if ($value is null) {
        throw new EvalUntypedArgumentException($name, 1);
      }
      return new MutableAtom($value);
    },
  );
}

function is_atom_function(): (Symbol, FunctionDefinition) {
  $name = 'atom?';
  return named_function(
    $name,
    $arguments ==> {
      $maybe_atom = idx($arguments, 1);
      if ($maybe_atom is null) {
        throw new EvalUntypedArgumentException($name, 1);
      }
      return new BoolAtom($maybe_atom is MutableAtom);
    },
  );
}

function deref_function(): (Symbol, FunctionDefinition) {
  $name = 'deref';
  return named_function(
    $name,
    $arguments ==> {
      $atom = idx($arguments, 1);
      if (!$atom is MutableAtom) {
        throw new EvalTypedArgumentException(
          $name,
          1,
          MutableAtom::class,
          $atom,
        );
      }
      return $atom->value;
    },
  );
}

function reset_function(): (Symbol, FunctionDefinition) {
  $name = 'reset!';
  return named_function(
    $name,
    $arguments ==> {
      $atom = idx($arguments, 1);
      if (!$atom is MutableAtom) {
        throw new EvalTypedArgumentException(
          $name,
          1,
          MutableAtom::class,
          $atom,
        );
      }
      $new_value = idx($arguments, 2);
      if ($new_value is null) {
        throw new EvalUntypedArgumentException($name, 2);
      }
      return $atom->reset($new_value);
    },
  );
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

function enforce_arity(
  string $name,
  int $max_num_arguments,
  vec<Form> $arguments,
): void {
  if (C\count($arguments) - 1 > $max_num_arguments) {
    throw new EvalArityException($name, $max_num_arguments, $arguments);
  }
}
