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

my %special_forms = (
    'def!' => \&special_def,
    'let*' => \&special_let,

    'do'  => \&special_do,
    'if'  => \&special_if,
    'fn*' => \&special_fn,

    'quasiquote' => \&special_quasiquote,
    'quote'      => \&special_quote,

    'defmacro!' => \&special_defmacro,
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
        if ( $f->isa('Mal::Macro') ) {
            @_ = ( $f->(@args), $env );
            goto &EVAL;
        }
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

sub special_quote {
    my ( $env, $quoted ) = @_;
    return $quoted;
}

sub special_quasiquote {
    my ( $env, $quoted ) = @_;
    @_ = ( quasiquote($quoted), $env );
    goto &EVAL;
}

sub special_defmacro {
    my ( $env, $sym, $val ) = @_;
    return $env->set( ${$sym}, Mal::Macro->new( EVAL( $val, $env )->clone ) );
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
while ( defined( my $line = mal_readline('user> ') ) ) {
    eval {
        print REP($line), "\n" or die $ERRNO;
        1;
    } or do {
        my $err = $EVAL_ERROR;
        print 'Error: ', $err or die $ERRNO;
    };
}
