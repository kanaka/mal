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
    symbol_function(),
    keyword_function(),
    list_function(),
    vector_function(),
    hashmap_function(),
    assoc_function(),
    dissoc_function(),
    get_function(),
    contains_function(),
    keys_function(),
    vals_function(),
    is_empty_function(),
    count_function(),
    equals_function(),
    read_string_function(),
    slurp(),
    readline_function(),
    eval_function($environment),
    atom_function(),
    deref_function(),
    reset_function(),
    apply_function(),
    cons_function(),
    conj_function(),
    concat_function(),
    nth_function(),
    rest_function(),
    seq_function(),
    throw_function(),
    map_function(),
    is_nil_function(),
    is_true_function(),
    is_false_function(),
    is_keyword_function(),
    is_symbol_function(),
    is_list_function(),
    is_vector_function(),
    is_sequential_function(),
    is_map_function(),
    is_atom_function(),
    is_string_function(),
    is_number_function(),
    is_fn_function(),
    is_macro_function(),
    meta_function(),
    with_meta_function(),
    time_ms_function(),
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
        throw new TypedArgumentException($name, 1, Number::class, $a);
      }
      $b = idx($arguments, 2);
      if (!$b is Number) {
        throw new TypedArgumentException($name, 2, Number::class, $b);
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

function symbol_function(): (Symbol, FunctionDefinition) {
  $name = 'symbol';
  return named_function(
    $name,
    $arguments ==> {
      $string = idx($arguments, 1);
      if (!$string is StringAtom) {
        throw new TypedArgumentException($name, 1, StringAtom::class, $string);
      }
      return new Symbol($string->value);
    },
  );
}

function keyword_function(): (Symbol, FunctionDefinition) {
  $name = 'keyword';
  return named_function(
    $name,
    $arguments ==> {
      $string = idx($arguments, 1);
      if ($string is Keyword) {
        return $string;
      }
      if (!$string is StringAtom) {
        throw new TypedArgumentException($name, 1, StringAtom::class, $string);
      }
      return new Keyword($string->value);
    },
  );
}

function list_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'list',
    $arguments ==> Vec\drop($arguments, 1)
      |> new ListForm($$),
  );
}

function vector_function(): (Symbol, FunctionDefinition) {
  return named_function(
    'vector',
    $arguments ==> Vec\drop($arguments, 1)
      |> new VectorForm($$),
  );
}

function hashmap_function(): (Symbol, FunctionDefinition) {
  $name = 'hash-map';
  return named_function(
    $name,
    $arguments ==> Vec\drop($arguments, 1)
      |> read_map_pairs($name, $$)
      |> pairs_to_map($$),
  );
}

function assoc_function(): (Symbol, FunctionDefinition) {
  $name = 'assoc';
  return named_function(
    $name,
    $arguments ==> {
      $map = idx($arguments, 1);
      if (!$map is HashMapForm) {
        throw new TypedArgumentException($name, 1, HashMapForm::class, $map);
      }
      return Vec\drop($arguments, 2)
        |> read_map_pairs($name, $$)
        |> Vec\concat(children_pairs($map), $$)
        |> pairs_to_map($$);
    },
  );
}

function dissoc_function(): (Symbol, FunctionDefinition) {
  $name = 'dissoc';
  return named_function(
    $name,
    $arguments ==> {
      $map = idx($arguments, 1);
      if (!$map is HashMapForm) {
        throw new TypedArgumentException($name, 1, HashMapForm::class, $map);
      }
      $keys_to_remove = Vec\drop($arguments, 2)
        |> Vec\map_with_key(
          $$,
          ($index, $key) ==> {
            if (!$key is Key) {
              throw new TypeException($name, Key::class, $key);
            }
            return key_to_string($key);
          },
        )
        |> keyset($$);
      if (C\is_empty($keys_to_remove)) {
        throw new MissingArgumentException($name, 2, 'a key to remove');
      }
      return new HashMapForm(
        Dict\filter_keys(
          $map->map,
          $key ==> !C\contains($keys_to_remove, $key),
        ),
      );
    },
  );
}

function read_map_pairs(string $name, vec<Form> $arguments): vec<(Key, Form)> {
  return read_pairs(
    $arguments,
    $key ==> {
      if (!$key is Key) {
        throw new EvalException(
          Str\format(
            '`%s` expected a key, but got `%s`',
            $name,
            pr_str($key, true),
          ),
        );
      }
      return $key;
    },
    $key ==> new EvalException(
      Str\format(
        '`%s` expected a value for key `%s`',
        $name,
        pr_str($key, true),
      ),
    ),
  );
}

function get_function(): (Symbol, FunctionDefinition) {
  return map_access_function('get', ($map, $key) ==> {
    $value = idx($map->map, $key);
    if ($value is null) {
      return new GlobalNil();
    }
    return $value;
  });
}

function contains_function(): (Symbol, FunctionDefinition) {
  return map_access_function(
    'contains?',
    ($map, $key) ==> new BoolAtom(C\contains_key($map->map, $key)),
  );
}

function map_access_function(
  string $name,
  (function(HashMapForm, string): Form) $access,
): (Symbol, FunctionDefinition) {
  return named_function(
    $name,
    $arguments ==> {
      $map = idx($arguments, 1);
      if ($map is GlobalNil) {
        return new GlobalNil();
      }
      if (!$map is HashMapForm) {
        throw new TypedArgumentException($name, 1, HashMapForm::class, $map);
      }
      $key = idx($arguments, 2);
      if (!$key is Key) {
        throw new TypedArgumentException($name, 2, Key::class, $key);
      }
      enforce_arity($name, 2, $arguments);
      $key_string = key_to_string($key);
      return $access($map, $key_string);
    },
  );
}

function keys_function(): (Symbol, FunctionDefinition) {
  $name = 'keys';
  return named_function(
    $name,
    $arguments ==> {
      $map = idx($arguments, 1);
      if (!$map is HashMapForm) {
        throw new TypedArgumentException($name, 1, HashMapForm::class, $map);
      }
      enforce_arity($name, 1, $arguments);
      return Vec\keys($map->map)
        |> Vec\map($$, $key ==> string_to_key($key))
        |> new ListForm($$);
    },
  );
}

function vals_function(): (Symbol, FunctionDefinition) {
  $name = 'vals';
  return named_function(
    $name,
    $arguments ==> {
      $map = idx($arguments, 1);
      if (!$map is HashMapForm) {
        throw new TypedArgumentException($name, 1, HashMapForm::class, $map);
      }
      enforce_arity($name, 1, $arguments);
      return new ListForm(vec($map->map));
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
        throw new TypedArgumentException($name, 1, ListLikeForm::class, $list);
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
        throw new TypedArgumentException($name, 1, ListLikeForm::class, $list);
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
      return equals_children_pairs($a->children, $b->children);
    }
  } else if ($a is HashMapForm) {
    if ($b is HashMapForm) {
      return equals_children_pairs($a->map, $b->map);
    }
  } else if ($a is FunctionDefinition) {
    if ($b is FunctionDefinition) {
    }
  } else {
    throw new EvalException("Unsupported form type in =");
  }
  return false;
}

function equals_children_pairs<TKey as arraykey>(
  KeyedContainer<TKey, Form> $children_pairs_a,
  KeyedContainer<TKey, Form> $children_pairs_b,
): bool {
  if (C\count($children_pairs_a) !== C\count($children_pairs_b)) {
    return false;
  }
  return C\every(Vec\map_with_key(
    $children_pairs_a,
    ($key_a, $value_a) ==> {
      $value_b = idx($children_pairs_b, $key_a);
      return $value_b is nonnull && equals_impl($value_a, $value_b);
    },
  ));
}

function read_string_function(): (Symbol, FunctionDefinition) {
  $name = 'read-string';
  return named_function(
    $name,
    $arguments ==> {
      $string = idx($arguments, 1);
      if (!$string is StringAtom) {
        throw new TypedArgumentException($name, 1, StringAtom::class, $string);
      }
      enforce_arity($name, 1, $arguments);
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
        throw new TypedArgumentException($name, 1, StringAtom::class, $string);
      }
      enforce_arity($name, 1, $arguments);
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

function readline_function(): (Symbol, FunctionDefinition) {
  $name = 'readline';
  return named_function(
    $name,
    $arguments ==> {
      $prompt = idx($arguments, 1);
      if (!$prompt is StringAtom) {
        throw new TypedArgumentException($name, 1, StringAtom::class, $prompt);
      }
      return \HH\Asio\join(
        async {
          echo $prompt->value;
          $cli_input = IO\request_input();
          $read_input = await $cli_input->readLineAsync();
          if ($cli_input->isEndOfFile()) {
            return new GlobalNil();
          }
          return new StringAtom(Regex\replace($read_input, re"/\r?\n$/", ''));
        },
      );
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
        throw new MissingArgumentException($name, 1);
      }
      enforce_arity($name, 1, $arguments);
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
        throw new MissingArgumentException($name, 1);
      }
      enforce_arity($name, 1, $arguments);
      return new MutableAtom($value);
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
        throw new TypedArgumentException($name, 1, MutableAtom::class, $atom);
      }
      enforce_arity($name, 1, $arguments);
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
        throw new TypedArgumentException($name, 1, MutableAtom::class, $atom);
      }
      $new_value = idx($arguments, 2);
      if ($new_value is null) {
        throw new MissingArgumentException($name, 2);
      }
      enforce_arity($name, 2, $arguments);
      return $atom->reset($new_value);
    },
  );
}

function apply_function(): (Symbol, FunctionDefinition) {
  $name = 'apply';
  return named_function(
    $name,
    $arguments ==> {
      $function = idx($arguments, 1);
      if (!$function is FunctionLike) {
        throw new TypedArgumentException(
          $name,
          1,
          FunctionLike::class,
          $function,
        );
      }
      $last_index = C\count($arguments) - 1;
      $list_argument = idx($arguments, $last_index);
      if (!$list_argument is ListLikeForm) {
        throw new TypedArgumentException(
          $name,
          $last_index,
          ListLikeForm::class,
          $list_argument,
        );
      }
      $prepend_arguments = Vec\slice($arguments, 1, C\count($arguments) - 2);
      $callable = unwrap_tco($function)->function;
      return $callable(
        Vec\concat($prepend_arguments, $list_argument->children),
      );
    },
  );
}

function cons_function(): (Symbol, FunctionDefinition) {
  $name = 'cons';
  return named_function(
    $name,
    $arguments ==> {
      $prepended_item = idx($arguments, 1);
      if ($prepended_item is null) {
        throw new MissingArgumentException($name, 1);
      }
      $list = idx($arguments, 2);
      if (!$list is ListLikeForm) {
        throw new TypedArgumentException($name, 2, ListLikeForm::class, $list);
      }
      enforce_arity($name, 2, $arguments);
      return new ListForm(Vec\concat(vec[$prepended_item], $list->children));
    },
  );
}

function conj_function(): (Symbol, FunctionDefinition) {
  $name = 'conj';
  return named_function(
    $name,
    $arguments ==> {
      $list = idx($arguments, 1);
      if (!$list is ListLikeForm) {
        throw new TypedArgumentException($name, 1, ListLikeForm::class, $list);
      }
      $rest_arguments = Vec\slice($arguments, 2);
      if ($list is ListForm) {
        return new ListForm(
          Vec\concat(Vec\reverse($rest_arguments), $list->children),
        );
      }
      if ($list is VectorForm) {
        return new VectorForm(Vec\concat($list->children, $rest_arguments));
      }
      invariant(false, 'Unsupported subtype of ListLikeForm');
    },
  );
}

function concat_function(): (Symbol, FunctionDefinition) {
  $name = 'concat';
  return named_function(
    $name,
    $arguments ==> {
      $concatted = Vec\flatten(Vec\map_with_key(
        Vec\drop($arguments, 1),
        ($index, $list) ==> {
          if (!$list is ListLikeForm) {
            throw new TypedArgumentException(
              $name,
              $index + 1,
              ListLikeForm::class,
              $list,
            );
          }
          return $list->children;
        },
      ));
      return new ListForm($concatted);
    },
  );
}

function nth_function(): (Symbol, FunctionDefinition) {
  $name = 'nth';
  return named_function(
    $name,
    $arguments ==> {
      $list = idx($arguments, 1);
      if (!$list is ListLikeForm) {
        throw new TypedArgumentException($name, 1, ListLikeForm::class, $list);
      }
      $index = idx($arguments, 2);
      if (!$index is Number) {
        throw new TypedArgumentException($name, 2, Number::class, $index);
      }
      enforce_arity($name, 2, $arguments);
      $value = idx($list->children, $index->value);
      if ($value is null) {
        throw new EvalException(
          "Index `$index->value` out of bounds in list ".pr_str($list, true),
        );
      }
      return $value;
    },
  );
}

function rest_function(): (Symbol, FunctionDefinition) {
  $name = 'rest';
  return named_function(
    $name,
    $arguments ==> {
      $list = idx($arguments, 1);
      if ($list is GlobalNil) {
        return new ListForm(vec[]);
      }
      if (!$list is ListLikeForm) {
        throw new TypedArgumentException($name, 1, ListLikeForm::class, $list);
      }
      enforce_arity($name, 1, $arguments);
      return new ListForm(Vec\drop($list->children, 1));
    },
  );
}

function seq_function(): (Symbol, FunctionDefinition) {
  $name = 'seq';
  return named_function(
    $name,
    $arguments ==> {
      $argument = idx($arguments, 1);
      if ($argument is null) {
        throw new MissingArgumentException($name, 1, $argument);
      }
      enforce_arity($name, 1, $arguments);
      if ($argument is ListForm) {
        return C\is_empty($argument->children) ? new GlobalNil() : $argument;
      }
      if ($argument is VectorForm) {
        return C\is_empty($argument->children)
          ? new GlobalNil()
          : new ListForm($argument->children);
      }
      if ($argument is StringAtom) {
        return Str\is_empty($argument->value)
          ? new GlobalNil()
          : new ListForm(Vec\map(
            Str\split($argument->value, ''),
            $character ==> new StringAtom($character),
          ));
      }
      if ($argument is GlobalNil) {
        return new GlobalNil();
      }
      throw new TypedArgumentException($name, 1, ListForm::class, $argument);
    },
  );
}

function throw_function(): (Symbol, FunctionDefinition) {
  $name = 'throw';
  return named_function(
    $name,
    $arguments ==> {
      $thrown = idx($arguments, 1);
      if ($thrown is null) {
        throw new MissingArgumentException($name, 1);
      }

      enforce_arity($name, 1, $arguments);
      throw new ThrownException($thrown);
    },
  );
}

function map_function(): (Symbol, FunctionDefinition) {
  $name = 'map';
  return named_function(
    $name,
    $arguments ==> {
      $function = idx($arguments, 1);
      if (!$function is FunctionLike) {
        throw new TypedArgumentException(
          $name,
          1,
          FunctionLike::class,
          $function,
        );
      }
      $list = idx($arguments, 2);
      if (!$list is ListLikeForm) {
        throw new TypedArgumentException($name, 2, ListLikeForm::class, $list);
      }
      enforce_arity($name, 2, $arguments);
      $callable = unwrap_tco($function)->function;
      return new ListForm(
        Vec\map($list->children, $item ==> $callable(vec[$function, $item])),
      );
    },
  );
}


function is_nil_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('nil?', $value ==> $value is GlobalNil);
}

function is_true_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function(
    'true?',
    $value ==> $value is BoolAtom && $value->value,
  );
}

function is_false_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function(
    'false?',
    $value ==> $value is BoolAtom && !$value->value,
  );
}

function is_symbol_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('symbol?', $value ==> $value is Symbol);
}

function is_keyword_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('keyword?', $value ==> $value is Keyword);
}

function is_list_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('list?', $value ==> $value is ListForm);
}

function is_vector_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('vector?', $value ==> $value is VectorForm);
}

function is_sequential_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function(
    'sequential?',
    $value ==> $value is ListLikeForm,
  );
}

function is_map_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('map?', $value ==> $value is HashMapForm);
}

function is_atom_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('atom?', $value ==> $value is MutableAtom);
}

function is_string_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('string?', $value ==> $value is StringAtom);
}

function is_number_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function('number?', $value ==> $value is Number);
}

function is_fn_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function(
    'fn?',
    $value ==> $value is FunctionLike &&
      !($value is FunctionWithTCODefinition && $value->is_macro),
  );
}

function is_macro_function(): (Symbol, FunctionDefinition) {
  return type_predicate_function(
    'macro?',
    $value ==> $value is FunctionWithTCODefinition && $value->is_macro,
  );
}

function type_predicate_function(
  string $name,
  (function(Form): bool) $check,
): (Symbol, FunctionDefinition) {
  return named_function(
    $name,
    $arguments ==> {
      $value = idx($arguments, 1);
      if ($value is null) {
        throw new MissingArgumentException($name, 1);
      }
      enforce_arity($name, 1, $arguments);
      return new BoolAtom($check($value));
    },
  );
}

function meta_function(): (Symbol, FunctionDefinition) {
  $name = 'meta';
  return named_function(
    $name,
    $arguments ==> {
      $argument = idx($arguments, 1);
      if (!$argument is WithMetadata) {
        throw new TypedArgumentException(
          $name,
          1,
          FunctionLike::class,
          $argument,
        );
      }
      enforce_arity($name, 1, $arguments);
      return $argument->metadata() ?? new GlobalNil();
    },
  );
}


function with_meta_function(): (Symbol, FunctionDefinition) {
  $name = 'with-meta';
  return named_function(
    $name,
    $arguments ==> {
      $argument = idx($arguments, 1);
      if (!$argument is WithMetadata) {
        throw new TypedArgumentException(
          $name,
          1,
          FunctionLike::class,
          $argument,
        );
      }
      $metadata = idx($arguments, 2);
      if ($metadata is null) {
        throw new MissingArgumentException($name, 2, $metadata);
      }
      enforce_arity($name, 2, $arguments);
      if ($argument is FunctionDefinition) {
        return new FunctionDefinition($argument->function, $metadata);
      }
      if ($argument is FunctionWithTCODefinition) {
        return new FunctionWithTCODefinition(
          $argument->is_macro,
          $argument->body,
          $argument->parameters,
          $argument->closed_over_environment,
          $argument->unoptimized,
          $metadata,
        );
      }
      if ($argument is ListForm) {
        return new ListForm($argument->children, $metadata);
      }
      if ($argument is VectorForm) {
        return new VectorForm($argument->children, $metadata);
      }
      if ($argument is HashMapForm) {
        return new HashMapForm($argument->map, $metadata);
      }
      invariant(false, 'Unsupported subtype of FunctionLike');
    },
  );
}

function time_ms_function(): (Symbol, FunctionDefinition) {
  $name = 'time-ms';
  return named_function(
    $name,
    $arguments ==> {
      enforce_arity($name, 0, $arguments);
      return new Number((int)(\microtime(true) * 1000));
    },
  );
}

function unwrap_tco(FunctionLike $function): FunctionDefinition {
  if ($function is FunctionWithTCODefinition) {
    return $function->unoptimized;
  }
  if ($function is FunctionDefinition) {
    return $function;
  }
  invariant(false, 'Unsupported subtype of FunctionLike');
}

function named_function(
  string $name,
  (function(vec<Form>): Form) $definition,
): (Symbol, FunctionDefinition) {
  return tuple(new Symbol($name), new FunctionDefinition($definition));
}

function enforce_arity(
  string $name,
  int $max_num_arguments,
  vec<Form> $arguments,
): void {
  if (C\count($arguments) - 1 > $max_num_arguments) {
    throw new ArityException($name, $max_num_arguments, $arguments);
  }
}
