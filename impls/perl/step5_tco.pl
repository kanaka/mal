#!/usr/bin/perl

use strict;
use warnings FATAL => 'recursion';
use File::Basename 'dirname';
use lib dirname(__FILE__);

use English '-no_match_vars';
use List::Util qw(pairs pairmap);

use Readline qw(mal_readline set_rl_mode);
use Types    qw(nil false);
use Reader   qw(read_str);
use Printer  qw(pr_str);
use Env;
use Core qw(%NS);

# False positives because of TCO.
## no critic (Subroutines::RequireArgUnpacking)

# read
sub READ {
    my $str = shift;
    return read_str($str);
}

# eval

my %special_forms = (
    'def!' => \&special_def,
    'let*' => \&special_let,

    'do'  => \&special_do,
    'if'  => \&special_if,
    'fn*' => \&special_fn,
);

sub EVAL {
    my ( $ast, $env ) = @_;

    my $dbgeval = $env->get('DEBUG-EVAL');
    if (    $dbgeval
        and not $dbgeval->isa('Mal::Nil')
        and not $dbgeval->isa('Mal::False') )
    {
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
    if ( $ast->isa('Mal::List') and @{$ast} ) {
        my ( $a0, @args ) = @{$ast};
        if ( $a0->isa('Mal::Symbol') and my $sf = $special_forms{ ${$a0} } ) {
            @_ = ( $env, @args );
            goto &{$sf};
        }
        my $f = EVAL( $a0, $env );
        @_ = map { EVAL( $_, $env ) } @args;
        goto &{$f};
    }
    return $ast;
}

sub special_def {
    my ( $env, $sym, $val ) = @_;
    return $env->set( ${$sym}, EVAL( $val, $env ) );
}

sub special_let {
    my ( $env, $bindings, $body ) = @_;
    my $let_env = Env->new($env);
    foreach my $pair ( pairs @{$bindings} ) {
        my ( $k, $v ) = @{$pair};
        $let_env->set( ${$k}, EVAL( $v, $let_env ) );
    }
    @_ = ( $body, $let_env );
    goto &EVAL;
}

sub special_do {
    my ( $env, @todo ) = @_;
    my $final = pop @todo;
    for (@todo) {
        EVAL( $_, $env );
    }
    @_ = ( $final, $env );
    goto &EVAL;
}

sub special_if {
    my ( $env, $if, $then, $else ) = @_;
    my $cond = EVAL( $if, $env );
    if ( not $cond->isa('Mal::Nil') and not $cond->isa('Mal::False') ) {
        @_ = ( $then, $env );
        goto &EVAL;
    }
    if ($else) {
        @_ = ( $else, $env );
        goto &EVAL;
    }
    return nil;
}

sub special_fn {
    my ( $env, $params, $body ) = @_;
    return Mal::Function->new(
        sub {
            @_ = ( $body, Env->new( $env, $params, \@_ ) );
            goto &EVAL;
        }
    );
}

# print
sub PRINT {
    my $exp = shift;
    return pr_str($exp);
}

# repl
my $repl_env = Env->new();

sub REP {
    my $str = shift;
    return PRINT( EVAL( READ($str), $repl_env ) );
}

# Command line arguments
if ( $ARGV[0] eq '--raw' ) {
    set_rl_mode('raw');
    shift @ARGV;
}

# core.pl: defined using perl
while ( my ( $k, $v ) = each %NS ) {
    $repl_env->set( $k, Mal::Function->new($v) );
}

# core.mal: defined using the language itself
REP(q[(def! not (fn* (a) (if a false true)))]);

while ( defined( my $line = mal_readline('user> ') ) ) {
    eval {
        print REP($line), "\n" or die $ERRNO;
        1;
    } or do {
        my $err = $EVAL_ERROR;
        print 'Error: ', $err or die $ERRNO;
    };
}
