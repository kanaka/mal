package REPL;

use Mo qw< xxx >;

use ReadLine;
use Reader;
use Eval;
use Printer;
use Env;
use Types;
use Core;

my $prompt = 'user> ';
my $env = Env->new->add(Core::ns);

$env->set('*ARGV*', list([map string($_), @ARGV[1..$#ARGV]]));
$env->set(eval => sub { Eval::eval($_[0], $env) });

# Define: `not`, 'load-file` and `cond`:
rep('(def! not (fn* (a) (if a false true)))');
rep('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))');
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

sub repl {
    while (defined (my $line = readline($prompt, $env))) {
        try($line) if length $line;
    }
    print "\n";
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
