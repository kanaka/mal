use v5.18;

use Term::ReadLine;
my $tty = Term::ReadLine->new('');

use Reader;
use Printer;

my $prompt = 'user> ';

sub Read {
    my ($line) = @_;
    Reader::read_str($line);
}

sub Eval {
    my ($line) = @_;
    return $line;
}

sub Print {
    my ($form) = @_;
    Printer::pr_str($form);
}

sub rep {
    my ($line) = @_;
    Print Eval Read $line;
}

my @tests = (
    q{{"a" {"b" {"c" 3}}}},
);


while (defined (my $line = $tty->readline($prompt))) {
# for my $line (@tests) {
    eval {
        print rep("$line") . "\n";
    };
    print "$@\n" if $@;
}
