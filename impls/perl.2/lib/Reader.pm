use v5.18;

package Reader;

use Mo;

has tokens => [];

sub read_str {
    my ($str) = @_;
    my $tokens = tokenize($str);
    my $reader = Reader->new(
        tokens => $tokens,
    );
    $reader->read_form;
}

my $lexer_re = qr/
    [\s,]*
    (
        ~@ |
        [\[\]{}()'`~^@] |
        "(?:
            \\. |
            [^\\"]
        )*"? |
        ;.* |
        [^\s\[\]{}('"`,;)]*
    )
/x;

sub tokenize {
    my ($str) = @_;
    my $tokens = [];
    my $pos = pos($str) = 0;
    while ($pos < length($str) and $str =~ /\G$lexer_re/gc) {
        push @$tokens, $1;
        $pos = pos($str);
    }
    die "Error at position $pos\n"
        if $pos < length($str);
    return $tokens;
}

sub read_form {
    my ($self) = @_;
    if ($self->{tokens}[0] eq '(') {
        $self->read_list;
    }
    else {
        $self->read_atom;
    }
}

sub read_list {
    my ($self) = @_;
    my $tokens = $self->{tokens};
    shift @$tokens;     # '('
    my $list = [];
    while (@$tokens > 0) {
        if ($tokens->[0] eq ')') {
            shift @$tokens;
            return $list;
        }
        push @$list, $self->read_form;
    }
    die "EOF\n";
}

sub read_atom {
    my ($self) = @_;
    my $atom = shift @{$self->{tokens}};
    return $atom;
}

1;
