import .Types;

Val pike_eval(string expr_str)
{
  program prog = compile_string("mixed tmp_func() { return (" + expr_str + "); }", "pike-eval");
  mixed v = prog()->tmp_func();
  return pike2mal(v);
}

private Val pike2mal(mixed v)
{
  if(stringp(v)) return String(v);
  if(intp(v)) return Number(v);
  if(arrayp(v))
  {
    array(Val) res = ({ });
    foreach(v, mixed e)
    {
      res += ({ pike2mal(e) });
    }
    return List(res);
  }
  if(mappingp(v))
  {
    array(Val) res = ({ });
    foreach(v; mixed k; mixed v)
    {
      res += ({ pike2mal(k), pike2mal(v) });
    }
    return Map(res);
  }
  return MAL_NIL;
}
