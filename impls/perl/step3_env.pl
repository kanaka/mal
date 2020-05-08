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
use types;
use reader;
use printer;
use env;

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
    #print "EVAL: " . printer::_pr_str($ast) . "\n";
    if (! $ast->isa('Mal::List')) {
        return eval_ast($ast, $env);
    }

    # apply list
    unless (@$ast) { return $ast; }
    given (${$ast->[0]}) {
        when ('def!') {
	    my (undef, $sym, $val) = @$ast;
            return $env->set($sym, EVAL($val, $env));
        }
        when ('let*') {
	    my (undef, $bindings, $body) = @$ast;
            my $let_env = Mal::Env->new($env);
	    foreach my $pair (pairs @$bindings) {
		my ($k, $v) = @$pair;
                $let_env->set($k, EVAL($v, $let_env));
            }
            return EVAL($body, $let_env);
        }
        default {
            my @el = @{eval_ast($ast, $env)};
            my $f = shift @el;
            return &$f(@el);
        }
    }
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

$repl_env->set(Mal::Symbol->new('+'),
	       sub { Mal::Integer->new(${$_[0]} + ${$_[1]}) } );
$repl_env->set(Mal::Symbol->new('-'),
	       sub { Mal::Integer->new(${$_[0]} - ${$_[1]}) } );
$repl_env->set(Mal::Symbol->new('*'),
	       sub { Mal::Integer->new(${$_[0]} * ${$_[1]}) } );
$repl_env->set(Mal::Symbol->new('/'),
	       sub { Mal::Integer->new(${$_[0]} / ${$_[1]}) } );

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
