use strict;
use warnings FATAL => qw(all);
use readline qw(readline);
use feature qw(switch);

use reader;
use printer;

# read
sub READ {
    my $str = shift;
    return reader::read_str($str);
}

# eval
sub EVAL {
    my($ast, $env) = @_;
    return $ast;
}

# print
sub PRINT {
    my $exp = shift;
    return printer::_pr_str($exp);
}

# repl
sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), {}));
}

while (1) {
    my $line = readline("user> ");
    if (! defined $line) { last; }
    eval {
        use autodie; # always "throw" errors
        print(REP($line), "\n");
        1;
    }; 
    if (my $err = $@) {
        chomp $err;
        print "Error: $err\n";
    }
}
