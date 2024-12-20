#!/usr/bin/perl

use strict;
use warnings;
use File::Basename 'dirname';
use lib dirname(__FILE__);

use English '-no_match_vars';

use Readline qw(mal_readline set_rl_mode);

# read
sub READ {
    my $str = shift;
    return $str;
}

# eval
sub EVAL {
    my ($ast) = @_;
    return $ast;
}

# print
sub PRINT {
    my $exp = shift;
    return $exp;
}

# repl
sub REP {
    my $str = shift;
    return PRINT( EVAL( READ($str) ) );
}

# Command line arguments
if ( $ARGV[0] eq '--raw' ) {
    set_rl_mode('raw');
    shift @ARGV;
}

while ( defined( my $line = mal_readline('user> ') ) ) {
    print REP($line), "\n" or die $ERRNO;
}
