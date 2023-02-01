use v5.18;

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

while (print $prompt and defined (my $line = <>)) {
    print rep $line;
}
