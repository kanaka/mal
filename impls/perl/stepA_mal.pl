#!/usr/bin/perl

use strict;
use warnings FATAL => "recursion";
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);
use File::Basename 'dirname';
use lib dirname(__FILE__);

use List::Util   qw(pairs pairmap);
use Scalar::Util qw(blessed);

use readline qw(mal_readline set_rl_mode);
use types    qw($nil $true $false);
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
    return @$ast && $ast->[0]->isa('Mal::Symbol') && ${ $ast->[0] } eq $sym;
}

sub quasiquote_loop {
    my ($ast) = @_;
    my $res = Mal::List->new( [] );
    foreach my $elt ( reverse @$ast ) {
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
    elsif ( $ast->isa('Mal::HashMap') or $ast->isa('Mal::Symbol') ) {
        return Mal::List->new( [ Mal::Symbol->new("quote"), $ast ] );
    }
    elsif ( !$ast->isa('Mal::List') ) {
        return $ast;
    }
    elsif ( starts_with( $ast, 'unquote' ) ) {
        return $ast->[1];
    }
    else {
        return quasiquote_loop($ast);
    }
}

sub EVAL {
    my ( $ast, $env ) = @_;

    my $dbgeval = $env->get('DEBUG-EVAL');
    if ( $dbgeval and $dbgeval ne $nil and $dbgeval ne $false ) {
        print "EVAL: " . pr_str($ast) . "\n";
    }

    if ( $ast->isa('Mal::Symbol') ) {
        my $val = $env->get($$ast);
        die "'$$ast' not found\n" unless $val;
        return $val;
    }
    elsif ( $ast->isa('Mal::Vector') ) {
        return ref($ast)->new( [ map { EVAL( $_, $env ) } @$ast ] );
    }
    elsif ( $ast->isa('Mal::HashMap') ) {
        return Mal::HashMap->new(
            { pairmap { $a => EVAL( $b, $env ) } %$ast } );
    }
    elsif ( !$ast->isa('Mal::List') ) {
        return $ast;
    }

    # apply list

    unless (@$ast) { return $ast; }
    my ($a0) = @$ast;
    given ( $a0->isa('Mal::Symbol') ? $$a0 : $a0 ) {
        when ('def!') {
            my ( undef, $sym, $val ) = @$ast;
            return $env->set( $$sym, EVAL( $val, $env ) );
        }
        when ('let*') {
            my ( undef, $bindings, $body ) = @$ast;
            my $let_env = Mal::Env->new($env);
            foreach my $pair ( pairs @$bindings ) {
                my ( $k, $v ) = @$pair;
                $let_env->set( $$k, EVAL( $v, $let_env ) );
            }
            @_ = ( $body, $let_env );
            goto &EVAL;
        }
        when ('quote') {
            return $ast->[1];
        }
        when ('quasiquote') {
            @_ = ( quasiquote( $ast->[1] ), $env );
            goto &EVAL;
        }
        when ('defmacro!') {
            my ( undef, $sym, $val ) = @$ast;
            return $env->set( $$sym,
                Mal::Macro->new( EVAL( $val, $env )->clone ) );
        }
        when ('try*') {
            my ( undef, $try, $catch ) = @$ast;
            local $@;
            my $ret = eval { EVAL( $try, $env ) };
            return $ret unless $@;
            if ( $catch && ${ $catch->[0] } eq 'catch*' ) {
                my ( undef, $binding, $body ) = @$catch;
                my $exc;
                if ( defined( blessed $@) && $@->isa('Mal::Type') ) {
                    $exc = $@;
                }
                else {
                    chomp( my $msg = $@ );
                    $exc = Mal::String->new($msg);
                }
                my $catch_env = Mal::Env->new( $env, [$binding], [$exc] );
                @_ = ( $body, $catch_env );
                goto &EVAL;
            }
            else {
                die $@;
            }
        }
        when ('do') {
            my ( undef, @todo ) = @$ast;
            my $last = pop @todo;
            map { EVAL( $_, $env ) } @todo;
            @_ = ( $last, $env );
            goto &EVAL;
        }
        when ('if') {
            my ( undef, $if, $then, $else ) = @$ast;
            my $cond = EVAL( $if, $env );
            if ( $cond eq $nil || $cond eq $false ) {
                @_ = ( $else // $nil, $env );
            }
            else {
                @_ = ( $then, $env );
            }
            goto &EVAL;
        }
        when ('fn*') {
            my ( undef, $params, $body ) = @$ast;
            return Mal::Function->new(
                sub {
                    #print "running fn*\n";
                    @_ = ( $body, Mal::Env->new( $env, $params, \@_ ) );
                    goto &EVAL;
                }
            );
        }
        default {
            my $f = EVAL( $a0, $env );
            my ( undef, @args ) = @$ast;
            if ( $f->isa('Mal::Macro') ) {
                @_ = ( &$f(@args), $env );
                goto &EVAL;
            }
            @_ = map { EVAL( $_, $env ) } @args;
            goto &$f;
        }
    }
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

# core.pl: defined using perl
while ( my ( $k, $v ) = each %NS ) {
    $repl_env->set( $k, Mal::Function->new($v) );
}
$repl_env->set( 'eval',
    Mal::Function->new( sub { EVAL( $_[0], $repl_env ) } ) );
my @_argv = map { Mal::String->new($_) } @ARGV[ 1 .. $#ARGV ];
$repl_env->set( '*ARGV*', Mal::List->new( \@_argv ) );

# core.mal: defined using the language itself
REP(q[(def! *host-language* "perl")]);
REP(q[(def! not (fn* (a) (if a false true)))]);
REP(
q[(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))]
);
REP(
q[(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))]
);

if ( @ARGV && $ARGV[0] eq "--raw" ) {
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
    if ( !defined $line ) { last; }
    do {
        local $@;
        my $ret;
        eval {
            print( REP($line), "\n" );
            1;
        } or do {
            my $err = $@;
            if ( defined( blessed $err) && $err->isa('Mal::BlankException') ) {

                # ignore and continue
            }
            elsif ( defined( blessed $err) && $err->isa('Mal::Type') ) {
                print "Error: " . pr_str($err) . "\n";
            }
            else {
                chomp $err;
                print "Error: $err\n";
            }
        };
    };
}
