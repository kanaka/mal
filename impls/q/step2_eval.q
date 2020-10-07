READ: {read_str x};

eval_ast: {[ast; env]; ty: first ast; $[ty = `symbol; actionordefault[last ast; env];
                                        ty = `list; (`list; EVAL[;env] peach last ast);
                                        ty = `vector; (`vector; EVAL[;env] peach last ast);
                                        ty = `hashmap; (`hashmap; maketable (
                                          {[x;env]; (`v`k!(
                                            enlist EVAL[first (x`v); env];
                                            enlist EVAL[first (x`k); env]))
                                          }[;env] each (value last ast)));
                                        ast]};
exec_or_fail: {[evald; syms]; x:.[first evald; tail evald]; $[x ~ 0; (`error, throw "Unknown function '", (PRINT first last syms), "'"); x] };
eval_list: {evald: last eval_ast[x; y]; $[100h = type first evald; exec_or_fail[evald; x]; (`error; throw "Unknown symbol '", (PRINT first last x), "'")]};
EVAL: {ty: first x; $[ty = `list; $[notempty last x; eval_list[x; y]; x]; eval_ast[x; y]]};

PRINT: {pr_str[x; 1b]};

show_or_ignore: { $[x ~ (); x; 1 x] };
rep: { @[show_or_ignore; PRINT EVAL[READ rl "user> "; repl_env]; {1 string x}]; 1"\n" };

main: { forever rep };

repl_env: ([name: ("a+"; "a-"; "a*"; "a/"; "d.")]
            fn: ({[x;y];(`number; (last x) + (last y))};
                 {[x;y];(`number; (last x) - (last y))};
                 {[x;y];(`number; (last x) * (last y))};
                 {[x;y];(`number; (last x) % (last y))};
                 0))

main`

