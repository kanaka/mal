use strict;
use warnings FATAL => qw(all);
use readline qw(readline);

# read
sub READ {
    my $str = shift;
    return $str;
}

# eval
sub EVAL {
    my($ast, $env) = @_;
    return eval($ast);
}

# print
sub PRINT {
    my $exp = shift;
    return $exp;
}

# repl
sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), {}));
}

while (1) {
    my $line = readline("user> ");
    if (! defined $line) { last; }
    print(REP($line), "\n");
}
