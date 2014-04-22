use strict;
use warnings FATAL => qw(all);
use readline qw(readline);
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
    } elsif (is_pair($ast->[0]) && _symbol_Q($ast->[0][0]) &&
             ${$ast->[0][0]} eq 'splice-unquote') {
        return List->new([Symbol->new("concat"),
                          $ast->[0][1],
                          quasiquote($ast->rest())]);
    } else {
        return List->new([Symbol->new("cons"),
                          quasiquote($ast->[0]),
                          quasiquote($ast->rest())]);
    }
}

sub is_macro_call {
    my ($ast, $env) = @_;
    if (_list_Q($ast) &&
        _symbol_Q($ast->[0]) &&
        $env->find(${$ast->[0]})) {
        my ($f) = $env->get(${$ast->[0]});
        if ((ref $f) =~ /^Function/) {
            return $f->{ismacro};
        }
    }
    return 0;
}

sub macroexpand {
    my ($ast, $env) = @_;
    while (is_macro_call($ast, $env)) {
        my $mac = $env->get(${$ast->[0]});
        $ast = $mac->apply($ast->rest());
    }
    return $ast;
}


sub eval_ast {
    my($ast, $env) = @_;
    given (ref $ast) {
        when (/^Symbol/) {
            $env->get($$ast);
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

    while (1) {

    #print "EVAL: " . printer::_pr_str($ast) . "\n";
    if (! _list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    # apply list
    $ast = macroexpand($ast, $env);
    if (! _list_Q($ast)) { return $ast; }

    my ($a0, $a1, $a2, $a3) = @$ast;
    given ((ref $a0) =~ /^Symbol/ ? $$a0 : $a0) {
        when (/^def!$/) {
            my $res = EVAL($a2, $env);
            return $env->set($$a1, $res);
        }
        when (/^let\*$/) {
            my $let_env = Env->new($env);
            for(my $i=0; $i < scalar(@{$a1}); $i+=2) {
                $let_env->set(${$a1->[$i]}, EVAL($a1->[$i+1], $let_env));
            }
            return EVAL($a2, $let_env);
        }
        when (/^quote$/) {
            return $a1;
        }
        when (/^quasiquote$/) {
            return EVAL(quasiquote($a1), $env);
        }
        when (/^defmacro!$/) {
            my $func = EVAL($a2, $env);
            $func->{ismacro} = 1;
            return $env->set($$a1, $func);
        }
        when (/^macroexpand$/) {
            return macroexpand($a1, $env);
        }
        when (/^do$/) {
            eval_ast($ast->slice(1, $#{$ast}-1), $env);
            $ast = $ast->[$#{$ast}];
        }
        when (/^if$/) {
            my $cond = EVAL($a1, $env);
            if ($cond eq $nil || $cond eq $false) {
                $ast = $a3 ? $a3 : $nil;
            } else {
                $ast = $a2;
            }
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
foreach my $n (%$core_ns) { $repl_env->set($n, $core_ns->{$n}); }
$repl_env->set('eval', sub { EVAL($_[0][0], $repl_env); });
my @_argv = map {String->new($_)}  @ARGV[1..$#ARGV];
$repl_env->set('*ARGV*', List->new(\@_argv));

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))");
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

if ($#ARGV > 0) {
    REP("(load-file \"" . $ARGV[0] . "\")");
    exit 0;
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
