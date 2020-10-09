READ: {read_str x};

eval_ast: {[ast; env]; ty: first ast; $[ty = `symbol; env_get[env; last ast];
                                        ty = `list; (`list; EVAL[;env] each last ast);
                                        ty = `vector; (`vector; EVAL[;env] each last ast);
                                        ty = `hashmap; (`hashmap; maketable (
                                          {[x;env]; (`v`k!(
                                            enlist EVAL[first (x`v); env];
                                            enlist EVAL[first (x`k); env]))
                                          }[;env] each (value last ast)));
                                        ast]};
exec_or_fail: {[evald; syms]; .[first evald; tail evald]};
eval_list: {evald: last eval_ast[x; y]; exec_or_fail[evald; x]};
eval_toplevel_list: {[ast; env];
  head:first last ast;
  hty:first head;
  $[hty = `symbol; $[
    strequals[last head; "def!"]; do_def[tail last ast; env];
    strequals[last head; "let*"]; do_letstar[tail last ast; env];
    eval_list[ast; env]]; eval_list[ast; env]]};

EVAL: {ty: first x; $[
  ty = `list; $[notempty last x; eval_toplevel_list[x; y]; x];
  eval_ast[x; y]]};

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
  EVAL[ast; new_env]};

show_or_ignore: { $[x ~ (); x; 1 x] };
rep: { show_or_ignore PRINT EVAL[READ rl "user> "; repl_env]; 1"\n" };

main: { forever rep };

xrepl_env: make_env ();

repl_env: env_set[env_set[env_set[env_set[xrepl_env;
              "+"; {[x;y];(`number; (last x) + (last y))}];
              "-"; {[x;y];(`number; (last x) - (last y))}];
              "*"; {[x;y];(`number; (last x) * (last y))}];
              "/"; {[x;y];(`number; (last x) % (last y))}];

main`

