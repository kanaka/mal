use strict;
use warnings;
use readline qw(readline);
use feature qw(switch);
use Data::Dumper;

use reader;
use printer;

# read
sub READ {
    my $str = shift;
    return reader::read_str($str);
}

# eval
sub eval_ast {
    my($ast, $env) = @_;
    given (ref $ast) {
        when (/^Symbol/) {
            if (exists $env->{$$ast}) {
                return $env->{$$ast};
            } else {
                die "'" . $$ast . "' not found";
            }
        }
        when (/^List/) {
            my @lst = map {EVAL($_, $env)} @$ast;
            return List->new(\@lst);
        }
        default {
            return $ast;
        }
    }
}

sub EVAL {
    my($ast, $env) = @_;
    #print "EVAL: " . printer::_pr_str($ast) . "\n";
    if (! ((ref $ast) =~ /^List/)) {
        return eval_ast($ast, $env);
    }

    # apply list
    my $el = eval_ast($ast, $env);
    my $f = $el->[0];
    return &{ $f }($el->rest());
}

# print
sub PRINT {
    my $exp = shift;
    return printer::_pr_str($exp);
}

# repl
my $repl_env = {};
sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), $repl_env));
}

$repl_env->{'+'} = sub { Integer->new(${$_[0][0]} + ${$_[0][1]}) };
$repl_env->{'-'} = sub { Integer->new(${$_[0][0]} - ${$_[0][1]}) };
$repl_env->{'*'} = sub { Integer->new(${$_[0][0]} * ${$_[0][1]}) };
$repl_env->{'/'} = sub { Integer->new(${$_[0][0]} / ${$_[0][1]}) };

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
