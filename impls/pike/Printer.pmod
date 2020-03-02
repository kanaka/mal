import .Types;

string pr_str(Val ast, bool print_readably)
{
  if(functionp(ast)) return "#<Fn>";
  return ast->to_string(print_readably);
}
