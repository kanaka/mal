#!/usr/bin/perl

use strict;
use warnings FATAL => 'recursion';
use File::Basename 'dirname';
use lib dirname(__FILE__);

use English '-no_match_vars';
use List::Util   qw(pairs pairmap);
use Scalar::Util qw(blessed);

use readline qw(mal_readline set_rl_mode);
use types    qw($nil $false);
use reader   qw(read_str);
use printer  qw(pr_str);
use env;
use core qw(%NS);

# False positives because of TCO.
## no critic (Subroutines::RequireArgUnpacking)

# read
sub READ {
    my $str = shift;
    return read_str($str);
}

# eval
sub starts_with {
    my ( $ast, $sym ) = @_;
    return @{$ast} && $ast->[0]->isa('Mal::Symbol') && ${ $ast->[0] } eq $sym;
}

sub quasiquote_loop {
    my ($ast) = @_;
    my $res = Mal::List->new( [] );
    foreach my $elt ( reverse @{$ast} ) {
        if ( $elt->isa('Mal::List') and starts_with( $elt, 'splice-unquote' ) )
        {
            $res =
              Mal::List->new( [ Mal::Symbol->new('concat'), $elt->[1], $res ] );
        }
        else {
            $res = Mal::List->new(
                [ Mal::Symbol->new('cons'), quasiquote($elt), $res ] );
        }
    }
    return $res;
}

sub quasiquote {
    my ($ast) = @_;
    if ( $ast->isa('Mal::Vector') ) {
        return Mal::List->new(
            [ Mal::Symbol->new('vec'), quasiquote_loop($ast) ] );
    }
    if ( $ast->isa('Mal::HashMap') or $ast->isa('Mal::Symbol') ) {
        return Mal::List->new( [ Mal::Symbol->new('quote'), $ast ] );
    }
    if ( $ast->isa('Mal::List') ) {
        if ( starts_with( $ast, 'unquote' ) ) {
            return $ast->[1];
        }
        return quasiquote_loop($ast);
    }
    return $ast;
}

## no critic (Subroutines::ProhibitExcessComplexity)
sub EVAL {
    my ( $ast, $env ) = @_;

    my $dbgeval = $env->get('DEBUG-EVAL');
    if ( $dbgeval and $dbgeval ne $nil and $dbgeval ne $false ) {
        print 'EVAL: ', pr_str($ast), "\n" or die $ERRNO;
    }

    if ( $ast->isa('Mal::Symbol') ) {
        return $env->get( ${$ast} ) // die "'${$ast}' not found\n";
    }
    if ( $ast->isa('Mal::Vector') ) {
        return Mal::Vector->new( [ map { EVAL( $_, $env ) } @{$ast} ] );
    }
    if ( $ast->isa('Mal::HashMap') ) {
        return Mal::HashMap->new(
            { pairmap { $a => EVAL( $b, $env ) } %{$ast} } );
    }
    $ast->isa('Mal::List') or return $ast;

    # apply list

    @{$ast} or return $ast;
    my ($a0) = @{$ast};
    if ( $a0->isa('Mal::Symbol') ) {
        if ( ${$a0} eq 'def!' ) {
            my ( undef, $sym, $val ) = @{$ast};
            return $env->set( ${$sym}, EVAL( $val, $env ) );
        }
        if ( ${$a0} eq 'let*' ) {
            my ( undef, $bindings, $body ) = @{$ast};
            my $let_env = Mal::Env->new($env);
            foreach my $pair ( pairs @{$bindings} ) {
                my ( $k, $v ) = @{$pair};
                $let_env->set( ${$k}, EVAL( $v, $let_env ) );
            }
            @_ = ( $body, $let_env );
            goto &EVAL;
        }
        if ( ${$a0} eq 'quote' ) {
            return $ast->[1];
        }
        if ( ${$a0} eq 'quasiquote' ) {
            @_ = ( quasiquote( $ast->[1] ), $env );
            goto &EVAL;
        }
        if ( ${$a0} eq 'defmacro!' ) {
            my ( undef, $sym, $val ) = @{$ast};
            return $env->set( ${$sym},
                Mal::Macro->new( EVAL( $val, $env )->clone ) );
        }
        if ( ${$a0} eq 'try*' ) {
            my ( undef, $try, $catch ) = @{$ast};
            if ($catch) {
                my ( undef, $binding, $body ) = @{$catch};
                if ( my $ret = eval { EVAL( $try, $env ) } ) {
                    return $ret;
                }
                my $exc = $EVAL_ERROR;
                if ( not blessed($exc) or not $exc->isa('Mal::Type') ) {
                    chomp $exc;
                    $exc = Mal::String->new($exc);
                }
                my $catch_env = Mal::Env->new( $env, [$binding], [$exc] );
                @_ = ( $body, $catch_env );
                goto &EVAL;
            }
            @_ = ( $try, $env );
            goto &EVAL;
        }
        if ( ${$a0} eq 'do' ) {
            my ( undef, @todo ) = @{$ast};
            my $final = pop @todo;
            for (@todo) {
                EVAL( $_, $env );
            }
            @_ = ( $final, $env );
            goto &EVAL;
        }
        if ( ${$a0} eq 'if' ) {
            my ( undef, $if, $then, $else ) = @{$ast};
            my $cond = EVAL( $if, $env );
            if ( $cond ne $nil and $cond ne $false ) {
                @_ = ( $then, $env );
                goto &EVAL;
            }
            if ( defined $else ) {
                @_ = ( $else, $env );
                goto &EVAL;
            }
            return $nil;
        }
        if ( ${$a0} eq 'fn*' ) {
            my ( undef, $params, $body ) = @{$ast};
            return Mal::Function->new(
                sub {
                    #print "running fn*\n";
                    @_ = ( $body, Mal::Env->new( $env, $params, \@_ ) );
                    goto &EVAL;
                }
            );
        }
    }
    my $f = EVAL( $a0, $env );
    my ( undef, @args ) = @{$ast};
    if ( $f->isa('Mal::Macro') ) {
        @_ = ( $f->(@args), $env );
        goto &EVAL;
    }
    @_ = map { EVAL( $_, $env ) } @args;
    goto &{$f};
}

# print
sub PRINT {
    my $exp = shift;
    return pr_str($exp);
}

# repl
my $repl_env = Mal::Env->new();

sub REP {
    my $str = shift;
    return PRINT( EVAL( READ($str), $repl_env ) );
}

# Command line arguments
if ( $ARGV[0] eq '--raw' ) {
    set_rl_mode('raw');
    shift @ARGV;
}
my $script_file = shift @ARGV;

# core.pl: defined using perl
while ( my ( $k, $v ) = each %NS ) {
    $repl_env->set( $k, Mal::Function->new($v) );
}
$repl_env->set( 'eval',
    Mal::Function->new( sub { EVAL( $_[0], $repl_env ) } ) );
$repl_env->set( '*ARGV*',
    Mal::List->new( [ map { Mal::String->new($_) } @ARGV ] ) );

# core.mal: defined using the language itself
REP(q[(def! *host-language* "perl")]);
REP(q[(def! not (fn* (a) (if a false true)))]);
REP(<<'EOF');
(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
EOF
REP(<<'EOF');
(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs)
(if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond"))
(cons 'cond (rest (rest xs)))))))
EOF

if ( defined $script_file ) {
    REP(qq[(load-file "$script_file")]);
    exit 0;
}
REP(q[(println (str "Mal [" *host-language* "]"))]);
while ( defined( my $line = mal_readline('user> ') ) ) {
    eval {
        print REP($line), "\n" or die $ERRNO;
        1;
    } or do {
        my $err = $EVAL_ERROR;
        next if defined blessed($err) and $err->isa('Mal::BlankException');
        if ( defined blessed($err) and $err->isa('Mal::Type') ) {
            $err = pr_str($err) . "\n";
        }
        print 'Error: ', $err or die $ERRNO;
    };
}
