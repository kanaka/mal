use strict;
use warnings;
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);
use File::Basename;
use lib dirname (__FILE__);

use Data::Dumper;
use List::Util qw(pairs pairmap);
use Scalar::Util qw(blessed);

use readline qw(mal_readline set_rl_mode);
use types qw($nil $true $false _sequential_Q _symbol_Q _list_Q _clone);
use reader;
use printer;
use env;
use core;

# read
sub READ {
    my $str = shift;
    return reader::read_str($str);
}

# eval
sub is_pair {
    my ($x) = @_;
    return _sequential_Q($x) && @$x;
}

sub quasiquote {
    my ($ast) = @_;
    if (!is_pair($ast)) {
        return Mal::List->new([Mal::Symbol->new("quote"), $ast]);
    } elsif (_symbol_Q($ast->[0]) && ${$ast->[0]} eq 'unquote') {
        return $ast->[1];
    } elsif (is_pair($ast->[0]) && _symbol_Q($ast->[0]->[0]) &&
             ${$ast->[0]->[0]} eq 'splice-unquote') {
        return Mal::List->new([Mal::Symbol->new("concat"),
                          $ast->[0]->[1],
                          quasiquote($ast->rest())]);
    } else {
        return Mal::List->new([Mal::Symbol->new("cons"),
                          quasiquote($ast->[0]),
                          quasiquote($ast->rest())]);
    }
}

sub is_macro_call {
    my ($ast, $env) = @_;
    if (_list_Q($ast) &&
        _symbol_Q($ast->[0]) &&
        $env->find($ast->[0])) {
        my ($f) = $env->get($ast->[0]);
        if ($f->isa('Mal::Function')) {
            return $f->{ismacro};
        }
    }
    return 0;
}

sub macroexpand {
    my ($ast, $env) = @_;
    while (is_macro_call($ast, $env)) {
        my @args = @$ast;
        my $mac = $env->get(shift @args);
        $ast = &$mac(@args);
    }
    return $ast;
}


sub eval_ast {
    my($ast, $env) = @_;
    if ($ast->isa('Mal::Symbol')) {
	return $env->get($ast);
    } elsif ($ast->isa('Mal::Sequence')) {
	return ref($ast)->new([ map { EVAL($_, $env) } @$ast ]);
    } elsif ($ast->isa('Mal::HashMap')) {
	return Mal::HashMap->new({ pairmap { $a => EVAL($b, $env) } %$ast });
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
    @$ast or return $ast;

    # apply list
    $ast = macroexpand($ast, $env);
    if (! _list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    my ($a0, $a1, $a2, $a3) = @$ast;
    if (!$a0) { return $ast; }
    given ($a0->isa('Mal::Symbol') ? $$a0 : $a0) {
        when ('def!') {
            my $res = EVAL($a2, $env);
            return $env->set($a1, $res);
        }
        when ('let*') {
            my $let_env = Mal::Env->new($env);
	    foreach my $pair (pairs @$a1) {
		my ($k, $v) = @$pair;
                $let_env->set($k, EVAL($v, $let_env));
            }
            $ast = $a2;
            $env = $let_env;
            # Continue loop (TCO)
        }
        when ('quote') {
            return $a1;
        }
        when ('quasiquote') {
            $ast = quasiquote($a1);
            # Continue loop (TCO)
        }
        when ('defmacro!') {
            my $func = _clone(EVAL($a2, $env));
            $func->{ismacro} = 1;
            return $env->set($a1, $func);
        }
        when ('macroexpand') {
            return macroexpand($a1, $env);
        }
        when ('try*') {
            do {
                local $@;
                my $ret;
                eval {
                    use autodie; # always "throw" errors
                    $ret = EVAL($a1, $env);
                     1;
                } or do {
                    my $err = $@;
                    if ($a2 && ${$a2->[0]} eq 'catch*') {
                        my $exc;
                        if (defined(blessed $err) && $err->isa('Mal::Type')) {
                            $exc = $err;
                        } else {
                            $exc = Mal::String->new(substr $err, 0, -1);
                        }
			my $catch_env =
			    Mal::Env->new($env, Mal::List->new([$a2->[1]]), 
					        Mal::List->new([$exc]));
                        return EVAL($a2->[2], $catch_env);
                    } else {
                        die $err;
                    }
                };
                return $ret;
            };
        }
        when ('do') {
            eval_ast($ast->slice(1, $#$ast-1), $env);
            $ast = $ast->[$#$ast];
            # Continue loop (TCO)
        }
        when ('if') {
            my $cond = EVAL($a1, $env);
            if ($cond eq $nil || $cond eq $false) {
                $ast = $a3 ? $a3 : $nil;
            } else {
                $ast = $a2;
            }
            # Continue loop (TCO)
        }
        when ('fn*') {
            return Mal::Function->new(\&EVAL, $a2, $env, $a1);
        }
        default {
            my @el = @{eval_ast($ast, $env)};
            my $f = shift @el;
            if ($f->isa('Mal::Function')) {
                $ast = $f->{ast};
                $env = $f->gen_env(\@el);
                # Continue loop (TCO)
            } else {
                return &$f(@el);
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
my $repl_env = Mal::Env->new();
sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), $repl_env));
}

# core.pl: defined using perl
foreach my $n (keys %core::ns) {
    $repl_env->set(Mal::Symbol->new($n), $core::ns{$n});
}
$repl_env->set(Mal::Symbol->new('eval'),
	       bless sub { EVAL($_[0], $repl_env); }, 'Mal::CoreFunction');
my @_argv = map {Mal::String->new($_)}  @ARGV[1..$#ARGV];
$repl_env->set(Mal::Symbol->new('*ARGV*'), Mal::List->new(\@_argv));

# core.mal: defined using the language itself
REP(q[(def! *host-language* "perl")]);
REP(q[(def! not (fn* (a) (if a false true)))]);
REP(q[(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))]);
REP(q[(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))]);

if (@ARGV && $ARGV[0] eq "--raw") {
    set_rl_mode("raw");
    shift @ARGV;
}
if (@ARGV) {
    REP(qq[(load-file "$ARGV[0]")]);
    exit 0;
}
REP(q[(println (str "Mal [" *host-language* "]"))]);
while (1) {
    my $line = mal_readline("user> ");
    if (! defined $line) { last; }
    do {
        local $@;
        my $ret;
        eval {
            print(REP($line), "\n");
            1;
        } or do {
            my $err = $@;
	    if (defined(blessed $err) && $err->isa('Mal::BlankException')) {
		# ignore and continue
	    } elsif (defined(blessed $err) && $err->isa('Mal::Type')) {
		print "Error: ".printer::_pr_str($err)."\n";
	    } else {
		chomp $err;
		print "Error: $err\n";
	    }
        };
    };
}
