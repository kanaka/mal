use strict;
use warnings FATAL => qw(all);
no if $] >= 5.018, warnings => "experimental::smartmatch";
use File::Basename;
use lib dirname (__FILE__);
use readline qw(mal_readline set_rl_mode);
use feature qw(switch);
use Data::Dumper;

use types qw(_list_Q);
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
	foreach my $k (keys %$ast) {
	    $new_hm->{$k} = EVAL($ast->get($k), $env);
	}
	return HashMap->new($new_hm);
    } else {
	return $ast;
    }
}

sub EVAL {
    my($ast, $env) = @_;
    #print "EVAL: " . printer::_pr_str($ast) . "\n";
    if (! _list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    # apply list
    my ($a0, $a1, $a2, $a3) = @$ast;
    if (!$a0) { return $ast; }
    given ($$a0) {
        when ('def!') {
            my $res = EVAL($a2, $env);
            return $env->set($a1, $res);
        }
        when ('let*') {
            my $let_env = Env->new($env);
            for(my $i=0; $i < scalar(@$a1); $i+=2) {
                $let_env->set($a1->[$i], EVAL($a1->[$i+1], $let_env));
            }
            return EVAL($a2, $let_env);
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
my $repl_env = Env->new();
sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), $repl_env));
}

$repl_env->set(Symbol->new('+'), sub { Integer->new(${$_[0]} + ${$_[1]}) } );
$repl_env->set(Symbol->new('-'), sub { Integer->new(${$_[0]} - ${$_[1]}) } );
$repl_env->set(Symbol->new('*'), sub { Integer->new(${$_[0]} * ${$_[1]}) } );
$repl_env->set(Symbol->new('/'), sub { Integer->new(${$_[0]} / ${$_[1]}) } );

if (scalar(@ARGV) > 0 && $ARGV[0] eq "--raw") {
    set_rl_mode("raw");
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
            if ($err->isa('BlankException')) {
		# ignore and continue
	    } else {
		chomp $err;
		print "Error: $err\n";
            }
        };
    };
}
