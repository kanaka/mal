use strict;
use warnings FATAL => qw(all);
use readline qw(mal_readline);
use feature qw(switch);
use Data::Dumper;

use types qw(_list_Q);
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
            my @lst = map {EVAL($_, $env)} @{$ast->{val}};
            return List->new(\@lst);
        }
        when (/^Vector/) {
            my @lst = map {EVAL($_, $env)} @{$ast->{val}};
            return Vector->new(\@lst);
        }
        when (/^HashMap/) {
            my $new_hm = {};
            foreach my $k (keys($ast->{val})) {
                $new_hm->{$k} = EVAL($ast->get($k), $env);
            }
            return HashMap->new($new_hm);
        }
        default {
            return $ast;
        }
    }
}

sub EVAL {
    my($ast, $env) = @_;
    #print "EVAL: " . printer::_pr_str($ast) . "\n";
    if (! _list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    # apply list
    my $el = eval_ast($ast, $env);
    my $f = $el->nth(0);
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

$repl_env->{'+'} = sub { Integer->new(${$_[0]->nth(0)} + ${$_[0]->nth(1)}) };
$repl_env->{'-'} = sub { Integer->new(${$_[0]->nth(0)} - ${$_[0]->nth(1)}) };
$repl_env->{'*'} = sub { Integer->new(${$_[0]->nth(0)} * ${$_[0]->nth(1)}) };
$repl_env->{'/'} = sub { Integer->new(${$_[0]->nth(0)} / ${$_[0]->nth(1)}) };

while (1) {
    my $line = mal_readline("user> ");
    if (! defined $line) { last; }
    do {
        local $@;
        my $ret;
        eval {
            use autodie; # always "throw" errors
            print(REP($line), "\n");
            1;
        } or do {
            my $err = $@;
            given (ref $err) {
                when (/^BlankException/) {
                    # ignore and continue
                }
                default {
                    chomp $err;
                    print "Error: $err\n";
                }
            }
        };
    };
}
