namespace Mal;

function pr_str(Form $mal_ast): string {
  if ($mal_ast is Number) {
    return (string)$mal_ast->value;
  } else if ($mal_ast is Keyword) {
    return ':'.$mal_ast->name;
  } else if ($mal_ast is Symbol) {
    return $mal_ast->name;
  } else if ($mal_ast is ListForm) {
    return print_children_form('(', $mal_ast->children, ')');
  } else if ($mal_ast is VectorForm) {
    return print_children_form('[', $mal_ast->children, ']');
  } else if ($mal_ast is HashMapForm) {
    return print_children_form('{', join_pairs(children_pairs($mal_ast)), '}');
  } else {
    invariant(false, 'Unhandled mal AST node type');
  }
}

function print_children_form(
  string $prefix,
  vec<Form> $children,
  string $suffix,
): string {
  $printed_children = Vec\map($children, $child ==> pr_str($child));
  return $prefix.Str\join($printed_children, ' ').$suffix;
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
