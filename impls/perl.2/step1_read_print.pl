use v5.10;

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


# for my $line (@tests) {
while (defined (my $line = $tty->readline($prompt))) {
    eval { print rep("$line") . "\n" };
    if (defined $@) {
        die $@ if $@ =~ /(^>>|^---\s| via package ")/;
        print "Error: $@\n";
    }
}
