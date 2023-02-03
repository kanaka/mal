use v5.18;

use FindBin qw($Bin);
use lib "$Bin/lib";

use ReadLine;
use Reader;
use Eval;
use Printer;
use Env;

my $prompt = 'user> ';

our @tests;
if (not @tests) {
    while (defined (my $line = readline($prompt))) {
        try($line) if length $line;
    }
}

sub try {
    my ($line) = @_;
    eval { print rep("$line") . "\n" };
    if ($@) {
        die $@ if $@ =~ /(^>>|^---\s| via package ")/;
        print "Error: $@\n";
    }
}

sub rep {
    my ($str) = @_;
    my $ast = Reader::read_str($str);
    $ast = Eval::eval($ast, $Env::ns);
    Printer::pr_str($ast);
}

1;
