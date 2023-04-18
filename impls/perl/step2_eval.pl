#!/usr/bin/perl

use strict;
use warnings;
use File::Basename 'dirname';
use lib dirname(__FILE__);

use English '-no_match_vars';
use List::Util qw(pairmap);

use Readline qw(mal_readline set_rl_mode);
use Types    qw();
use Reader   qw(read_str);
use Printer  qw(pr_str);

# read
sub READ {
    my $str = shift;
    return read_str($str);
}

# eval
sub EVAL {
    my ( $ast, $env ) = @_;

    #print 'EVAL: ', pr_str($ast), "\n" or die $ERRNO;

    if ( $ast->isa('Mal::Symbol') ) {
        return $env->{ ${$ast} } // die "'${$ast}' not found\n";
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
        my $f = EVAL( $a0, $env );
        return $f->( map { EVAL( $_, $env ) } @args );
    }
    return $ast;
}

# print
sub PRINT {
    my $exp = shift;
    return pr_str($exp);
}

# repl
my $repl_env = {
    q{+} => sub { Mal::Integer->new( ${ $_[0] } + ${ $_[1] } ) },
    q{-} => sub { Mal::Integer->new( ${ $_[0] } - ${ $_[1] } ) },
    q{*} => sub { Mal::Integer->new( ${ $_[0] } * ${ $_[1] } ) },
    q{/} => sub { Mal::Integer->new( ${ $_[0] } / ${ $_[1] } ) },
};

sub REP {
    my $str = shift;
    return PRINT( EVAL( READ($str), $repl_env ) );
}

# Command line arguments
if ( $ARGV[0] eq '--raw' ) {
    set_rl_mode('raw');
    shift @ARGV;
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
