READ: {read_str x};

EVAL: {x};

PRINT: {pr_str[x; 1b]};

show_or_ignore: { $[x ~ (); x; 1 x] };
rep: { show_or_ignore PRINT EVAL READ rl "user> "; 1"\n" };

main: { forever rep };


main`

