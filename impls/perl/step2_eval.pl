use strict;
use warnings;
use File::Basename;
use lib dirname (__FILE__);

use Data::Dumper;
use List::Util qw(pairmap);
use Scalar::Util qw(blessed);

use readline qw(mal_readline set_rl_mode);
use types;
use reader;
use printer;

# read
sub READ {
    my $str = shift;
    return reader::read_str($str);
}

# eval
sub EVAL {
    my($ast, $env) = @_;
    #print "EVAL: " . printer::_pr_str($ast) . "\n";

    if ($ast->isa('Mal::Symbol')) {
	return $env->{$$ast} // die "'$$ast' not found\n";
    } elsif ($ast->isa('Mal::Vector')) {
	return ref($ast)->new([ map { EVAL($_, $env) } @$ast ]);
    } elsif ($ast->isa('Mal::HashMap')) {
	return Mal::HashMap->new({ pairmap { $a => EVAL($b, $env) } %$ast });
    } elsif (! $ast->isa('Mal::List')) {
	return $ast;
    }

    # apply list

    unless (@$ast) { return $ast; }
    my ($a0) = @$ast;
    my $f = EVAL($a0, $env);
    my (undef, @args) = @$ast;
    return &$f(map { EVAL($_, $env) } @args);
}

# print
sub PRINT {
    my $exp = shift;
    return printer::_pr_str($exp);
}

# repl
my $repl_env = {
    '+' => sub { Mal::Integer->new(${$_[0]} + ${$_[1]}) },
    '-' => sub { Mal::Integer->new(${$_[0]} - ${$_[1]}) },
    '*' => sub { Mal::Integer->new(${$_[0]} * ${$_[1]}) },
    '/' => sub { Mal::Integer->new(${$_[0]} / ${$_[1]}) },
};

sub REP {
    my $str = shift;
    return PRINT(EVAL(READ($str), $repl_env));
}

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
