use strict;
use warnings FATAL => qw(all);
use readline qw(mal_readline);
use feature qw(switch);
use Data::Dumper;

use types qw($nil $true $false _list_Q);
use reader;
use printer;
use env;
use core qw($core_ns);

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
            $env->get($$ast);
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
    my ($a0, $a1, $a2, $a3) = @{$ast->{val}};
    given ((ref $a0) =~ /^Symbol/ ? $$a0 : $a0) {
        when (/^def!$/) {
            my $res = EVAL($a2, $env);
            return $env->set($$a1, $res);
        }
        when (/^let\*$/) {
            my $let_env = Env->new($env);
            for(my $i=0; $i < scalar(@{$a1->{val}}); $i+=2) {
                $let_env->set(${$a1->nth($i)}, EVAL($a1->nth($i+1), $let_env));
            }
            return EVAL($a2, $let_env);
        }
        when (/^do$/) {
            my $el = eval_ast($ast->rest(), $env);
            return $el->nth($#{$el->{val}});
        }
        when (/^if$/) {
            my $cond = EVAL($a1, $env);
            if ($cond eq $nil || $cond eq $false) {
                return $a3 ? EVAL($a3, $env) : $nil;
            } else {
                return EVAL($a2, $env);
            }
        }
        when (/^fn\*$/) {
            return sub {
                #print "running fn*\n";
                my $args = $_[0];
                return EVAL($a2, Env->new($env, $a1, $args));
            };
        }
        default {
            my $el = eval_ast($ast, $env);
            my $f = $el->nth(0);
            return &{ $f }($el->rest());
        }
    }
}

# print
sub PRINT {
    my $exp = shift;
    return printer::_pr_str($exp);
}

# repl
my $repl_env = Env->new();
sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), $repl_env));
}

# core.pl: defined using perl
foreach my $n (%$core_ns) { $repl_env->set($n, $core_ns->{$n}); }

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))");

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
