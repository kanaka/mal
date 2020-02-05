namespace Mal;

function pr_str(Form $mal_ast): string {
  if ($mal_ast is Number) {
    return (string)$mal_ast->value;
  } else if ($mal_ast is Symbol) {
    return $mal_ast->name;
  } else if ($mal_ast is ListForm) {
    $printed_children = Vec\map($mal_ast->children, $child ==> pr_str($child));
    return '('.Str\join($printed_children, ' ').')';
  } else {
    invariant(false, 'Unhandled mal AST node type');
  }
}
