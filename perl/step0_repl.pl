use strict;
use warnings FATAL => qw(all);
use File::Basename;
use lib dirname (__FILE__);
use readline qw(mal_readline set_rl_mode);

# read
sub READ {
    my $str = shift;
    return $str;
}

# eval
sub EVAL {
    my($ast, $env) = @_;
    return $ast;
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

if (scalar(@ARGV) > 0 && $ARGV[0] eq "--raw") {
    set_rl_mode("raw");
}
while (1) {
    my $line = mal_readline("user> ");
    if (! defined $line) { last; }
    print(REP($line), "\n");
}
