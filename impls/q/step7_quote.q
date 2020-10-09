READ: {read_str x};

eval_ast: {[ast; env]; ty: first ast; $[ty ~ `symbol; env_get[env; last ast];
                                        ty ~ `list; (`list; EVAL[;env] each last ast);
                                        ty ~ `vector; (`vector; EVAL[;env] each last ast);
                                        ty ~ `hashmap; (`hashmap; maketable (
                                          {[x;env]; (`v`k!(
                                            enlist EVAL[first (x`v); env];
                                            enlist EVAL[first (x`k); env]))
                                          }[;env] each (value last ast)));
                                        ast]};
exec_or_fail: {[evald; syms]; (first evald)[tail evald]};
eval_list: {evald: last eval_ast[x; y]; exec_or_fail[evald; x]};
eval_toplevel_list: {[ast; env];
  head:first last ast;
  hty:first head;
  $[0h = (type head);
    $[hty ~ `symbol; $[
      strequals[last head; "def!"]; do_def[tail last ast; env];
      strequals[last head; "let*"]; do_letstar[tail last ast; env];
      strequals[last head; "do"]; do_do[tail last ast; env];
      strequals[last head; "if"]; do_if[tail last ast; env];
      strequals[last head; "fn*"]; do_fnstar[tail last ast; env];
      strequals[last head; "eval"]; do_eval[tail last ast; env];
      strequals[last head; "swap!"]; do_swapbang[tail last ast; env];
      strequals[last head; "quote"]; first tail last ast;
      strequals[last head; "quasiquote"]; (`tco; do_quasiquote[first tail last ast]; env);
      strequals[last head; "quasiquoteexpand"]; do_quasiquote[first tail last ast];
      eval_list[ast; env]]; eval_list[ast; env]]; eval_list[ast; env]]};

EVAL: {[x;env];
  last first while_[{first x}; (1b; x; env); {[a];
    x: a @ 1;
    y: a @ 2;
    ty: first x;
    res:$[ty ~ `list;
      $[notempty last x; eval_toplevel_list[x; y]; x];
      eval_ast[x; y]];
    $[0h = (type res);
      $[`tco ~ (first res); (1b; res @ 1; res @ 2); (0b; res)];
      (0b; res)]}]};

PRINT: {pr_str[x; 1b]};

do_def: {[partial_ast; env];
  res:EVAL[last partial_ast; env];
  env_set[env; last first partial_ast; res];
  res};

do_letstar:{[partial_ast; env];
  new_env:make_env env;
  binding_list: last first partial_ast;
  ast: last partial_ast;
  accumulate[notempty; binding_list; {[x;env]; (env_set[env; last first x; EVAL[x @ 1; env]]; skip[2; x])}[;new_env]];
  (`tco; ast; new_env)};

do_do:{[partial_ast; env];
  last first accumulate[notempty; init partial_ast; {[asts; env]; (EVAL[first asts; env]; tail asts)}[;env]];
  (`tco; last partial_ast; env)};

do_if:{[partial_ast; env];
  cty:first EVAL[first partial_ast; env];
  $[not ((cty ~ `false) or (cty ~ `nil)); (`tco; partial_ast @ 1; env);
  $[2 = count partial_ast; (`nil; ()); (`tco; partial_ast @ 2; env)]]};

do_fnstar:{[partial_ast; env];
  {[args;argnames;body;env];
    new_env:make_env_with_binds[env; argnames; args];
    (`tco; body; new_env)}[; last first partial_ast; partial_ast @ 1; env]};

do_eval:{[partial_ast; env];
  ast: EVAL[first partial_ast; env];
  EVAL[ast; repl_env]};

do_swapbang:{[partial_ast; env];
  xs:EVAL[; env] each partial_ast;
  atom:last first xs;
  fn:xs @ 1;
  args:skip[2; xs];
  val:EVAL[fn ((enlist get_atom atom), args); env];
  set_atom[atom; val];
  val};

do_quasiquote:{[ast];
  qq:{[ast];
    first first while_[{notempty last x}; (list (); last ast); {[x];
      $[((first last last x) ~ `list) and issymbol[first last last last x; "splice-unquote"]; (list (symbol "concat"; (last last last x) @ 1; first x); init last x);
        (list (symbol "cons"; do_quasiquote[last last x]; first x); init last x)]}]};
  $[(first ast) ~ `list; $[issymbol[first last ast; "unquote"]; (last ast) @ 1; qq[ast]];
    (first ast) ~ `hashmap; list (symbol "quote"; ast);
    (first ast) ~ `symbol; list (symbol "quote"; ast);
    (first ast) ~ `vector; list (symbol "vec"; qq[ast]);
    ast]};

breakpoint: {[x]; break x};

show_or_ignore: { $[x ~ (); x; 1 x] };
rep: { @[show_or_ignore; PRINT EVAL[READ rl "user> "; repl_env]; {1 string x}]; 1"\n" };

withargs: {
  env_set[repl_env; "*ARGV*"; list str each tail .z.x];
  PRINT EVAL[(`list; ((`symbol; "load-file"); str first .z.x)); repl_env];
  exit 0};

repl:{
  env_set[repl_env; "*ARGV*"; (`list; ())];
  forever rep};

main: {
  EVAL[READ "(def! not (fn* (a) (if a false true)))"; repl_env];
  EVAL[READ "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"; repl_env];
  $[count .z.x > 0;
    $[strequals[first .z.x; "-repl"]; repl`; withargs`];
    repl`]};


xrepl_env: make_env ();

repl_env: add_core_ns env_set[env_set[env_set[env_set[xrepl_env;
              "+"; {(`number; (last (x @ 0)) + (last (x @ 1)))}];
              "-"; {(`number; (last (x @ 0)) - (last (x @ 1)))}];
              "*"; {(`number; (last (x @ 0)) * (last (x @ 1)))}];
              "/"; {(`number; (last (x @ 0)) % (last (x @ 1)))}];

main`

