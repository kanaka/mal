READ: {x};

EVAL: {x};

PRINT: {x};

rep: { 1 PRINT EVAL READ rl "user> "; 1"\n" };

main: { forever rep };


main`

