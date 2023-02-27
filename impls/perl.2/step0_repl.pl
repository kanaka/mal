use v5.12;

use Term::ReadLine;
my $tty = Term::ReadLine->new('');

my $prompt = 'user> ';

sub Read {
    my ($line) = @_;
    return $line;
}

sub Eval {
    my ($line) = @_;
    return $line;
}

sub Print {
    my ($line) = @_;
    return $line;
}

sub rep {
    my ($line) = @_;
    Print Eval Read $line;
}

while (defined (my $line = $tty->readline($prompt))) {
    print rep("$line") . "\n";
}
