use strict;
use warnings FATAL => qw(all);
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);
use File::Basename;
use lib dirname (__FILE__);

use Data::Dumper;
use List::Util qw(pairs pairmap);
use Scalar::Util qw(blessed);

use readline qw(mal_readline set_rl_mode);
use types qw($nil $true $false _list_Q);
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

    # apply list
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

# core.mal: defined using the language itself
REP(q[(def! not (fn* (a) (if a false true)))]);

if (@ARGV && $ARGV[0] eq "--raw") {
    set_rl_mode("raw");
}
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
	    } else {
		chomp $err;
		print "Error: $err\n";
            }
        };
    };
}
