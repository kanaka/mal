use strict;
use warnings FATAL => qw(all);
no if $] >= 5.018, warnings => "experimental::smartmatch";
use File::Basename;
use lib dirname (__FILE__);
use readline qw(mal_readline set_rl_mode);
use feature qw(switch);
use Data::Dumper;

use types qw($nil $true $false _sequential_Q _symbol_Q _list_Q);
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
sub is_pair {
    my ($x) = @_;
    return _sequential_Q($x) && scalar(@$x) > 0;
}

sub quasiquote {
    my ($ast) = @_;
    if (!is_pair($ast)) {
        return List->new([Symbol->new("quote"), $ast]);
    } elsif (_symbol_Q($ast->[0]) && ${$ast->[0]} eq 'unquote') {
        return $ast->[1];
    } elsif (is_pair($ast->[0]) && _symbol_Q($ast->[0]->[0]) &&
             ${$ast->[0]->[0]} eq 'splice-unquote') {
        return List->new([Symbol->new("concat"),
                          $ast->[0]->[1],
                          quasiquote($ast->rest())]);
    } else {
        return List->new([Symbol->new("cons"),
                          quasiquote($ast->[0]),
                          quasiquote($ast->rest())]);
    }
}

sub eval_ast {
    my($ast, $env) = @_;
    if ($ast->isa('Symbol')) {
	return $env->get($ast);
    } elsif ($ast->isa('List')) {
	my @lst = map {EVAL($_, $env)} @$ast;
	return List->new(\@lst);
    } elsif ($ast->isa('Vector')) {
	my @lst = map {EVAL($_, $env)} @$ast;
	return Vector->new(\@lst);
    } elsif ($ast->isa('HashMap')) {
	my $new_hm = {};
	foreach my $k (keys( %{ $ast->{val} })) {
	    $new_hm->{$k} = EVAL($ast->get($k), $env);
	}
	return HashMap->new($new_hm);
    } else {
	return $ast;
    }
}

sub EVAL {
    my($ast, $env) = @_;

    while (1) {

    #print "EVAL: " . printer::_pr_str($ast) . "\n";
    if (! _list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    # apply list
    my ($a0, $a1, $a2, $a3) = @$ast;
    if (!$a0) { return $ast; }
    given ($a0->isa('Symbol') ? $$a0 : $a0) {
        when (/^def!$/) {
            my $res = EVAL($a2, $env);
            return $env->set($a1, $res);
        }
        when (/^let\*$/) {
            my $let_env = Env->new($env);
            for(my $i=0; $i < scalar(@$a1); $i+=2) {
                $let_env->set($a1->[$i], EVAL($a1->[$i+1], $let_env));
            }
            $ast = $a2;
            $env = $let_env;
            # Continue loop (TCO)
        }
        when (/^quote$/) {
            return $a1;
        }
        when (/^quasiquote$/) {
            $ast = quasiquote($a1);
            # Continue loop (TCO)
        }
        when (/^do$/) {
            eval_ast($ast->slice(1, $#$ast-1), $env);
            $ast = $ast->[$#$ast];
            # Continue loop (TCO)
        }
        when (/^if$/) {
            my $cond = EVAL($a1, $env);
            if ($cond eq $nil || $cond eq $false) {
                $ast = $a3 ? $a3 : $nil;
            } else {
                $ast = $a2;
            }
            # Continue loop (TCO)
        }
        when (/^fn\*$/) {
            return Function->new(\&EVAL, $a2, $env, $a1);
        }
        default {
            my $el = eval_ast($ast, $env);
            my $f = $el->[0];
            if ((ref $f) =~ /^Function/) {
                $ast = $f->{ast};
                $env = $f->gen_env($el->rest());
                # Continue loop (TCO)
            } else {
                return &{ $f }($el->rest());
            }
        }
    }

    } # TCO while loop
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
foreach my $n (%$core_ns) {
    $repl_env->set(Symbol->new($n), $core_ns->{$n});
}
$repl_env->set(Symbol->new('eval'), sub { EVAL($_[0]->[0], $repl_env); });
my @_argv = map {String->new($_)}  @ARGV[1..$#ARGV];
$repl_env->set(Symbol->new('*ARGV*'), List->new(\@_argv));

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))");
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

if (scalar(@ARGV) > 0 && $ARGV[0] eq "--raw") {
    set_rl_mode("raw");
    shift @ARGV;
}
if (scalar(@ARGV) > 0) {
    REP("(load-file \"" . $ARGV[0] . "\")");
    exit 0;
}
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
