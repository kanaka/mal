namespace Mal;

function pr_str(Form $mal_ast, bool $print_readably = false): string {
  if ($mal_ast is Number) {
    return (string)$mal_ast->value;
  } else if ($mal_ast is Keyword) {
    return ':'.$mal_ast->name;
  } else if ($mal_ast is StringAtom) {
    return $print_readably
      ? print_string_readably($mal_ast->value)
      : $mal_ast->value;
  } else if ($mal_ast is Symbol) {
    return $mal_ast->name;
  } else if ($mal_ast is GlobalNil) {
    return "nil";
  } else if ($mal_ast is BoolAtom) {
    return $mal_ast->value ? 'true' : 'false';
  } else if ($mal_ast is ListForm) {
    return print_children_form('(', $mal_ast->children, ')', $print_readably);
  } else if ($mal_ast is VectorForm) {
    return print_children_form('[', $mal_ast->children, ']', $print_readably);
  } else if ($mal_ast is HashMapForm) {
    return print_children_form(
      '{',
      join_pairs(children_pairs($mal_ast)),
      '}',
      $print_readably,
    );
  } else {
    invariant(false, 'Unhandled mal AST node type');
  }
}

function print_children_form(
  string $prefix,
  vec<Form> $children,
  string $suffix,
  bool $print_readably,
): string {
  $printed_children = Vec\map(
    $children,
    $child ==> pr_str($child, $print_readably),
  );
  return $prefix.Str\join($printed_children, ' ').$suffix;
}

function print_string_readably(string $raw_string): string {
  return $raw_string
    |> Regex\replace_with($$, re"/\\\\|\"|\n/", $escape_sequence ==> {
      switch ($escape_sequence[0]) {
        case '\\':
          return '\\\\';
        case "\"":
          return '\"';
        case "\n":
        default: // exhaustive
          return '\n';
      }
    })
    |> '"'.$$.'"';
}

function join_pairs<T>(vec<(T, T)> $pairs): vec<T> {
  $joined = vec[];
  foreach ($pairs as $pair) {
    list($first, $second) = $pair;
    $joined[] = $first;
    $joined[] = $second;
  }
  return $joined;
}
