package REPL;

use Mo;

use ReadLine;
use Reader;
use Eval;
use Printer;
use Env;
use Types;
use Core;

my $prompt = 'user> ';
my $env = Env->new(
    binds => Core::binds(),
    exprs => Core::exprs(),
);

# Define: `not`
rep('(def! not (fn* (a) (if a false true)))');

sub repl {
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
    $ast = Eval::eval($ast, $env);
    Printer::pr_str($ast);
}

1;
