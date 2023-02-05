package Reader;

use Mo;

use Types;

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
    $_ = $self->{tokens}[0];
    /^\($/ ? $self->read_list('list', ')') :
    /^\[$/ ? $self->read_list('vector', ']') :
    /^\{$/ ? $self->read_hash('hash_map', '}') :
    /^'$/ ? $self->read_quote('quote') :
    /^`$/ ? $self->read_quote('quasiquote') :
    /^~$/ ? $self->read_quote('unquote') :
    /^~\@$/ ? $self->read_quote('splice_unquote') :
    /^\@$/ ? $self->read_quote('deref') :
    /^\^$/ ? $self->with_meta :
    $self->read_atom;
}

sub read_list {
    my ($self, $type, $end) = @_;
    my $tokens = $self->{tokens};
    shift @$tokens;
    my $list = $type->new([]);
    while (@$tokens > 0) {
        if ($tokens->[0] eq $end) {
            shift @$tokens;
            return $list;
        }
        push @$list, $self->read_form;
    }
    die "Reached end of input in 'read_list'";
}

sub read_hash {
    my ($self, $type, $end) = @_;
    my $tokens = $self->{tokens};
    shift @$tokens;
    my $hash = $type->new;
    while (@$tokens > 0) {
        if ($tokens->[0] eq $end) {
            shift @$tokens;
            return $hash;
        }
        my $key = $self->read_form;
        my $val = $self->read_form;
        $hash->{$$key} = $val;
    }
    die "Reached end of input in 'read_hash'";
}

my $string_re = qr/"((?:\\.|[^\\"])*)"/;
my $unescape = {
    'n' => "\n",
    't' => "\t",
    '"' => '"',
    '\\' => "\\",
};
sub read_atom {
    my ($self) = @_;
    my $atom = $_ = shift @{$self->{tokens}};

    if (/^"/) {
        s/^$string_re$/$1/ or
            die "Reached end of input looking for '\"'";
        s/\\([nt\"\\])/$unescape->{$1}/ge;
        return string->new($_);
    }
    return true if $_ eq 'true';
    return false if $_ eq 'false';
    return nil if $_ eq 'nil';
    return number->new($_) if /^-?\d+$/;
    return keyword->new($_) if /^:/;
    return symbol->new($_);
}

sub read_quote {
    my ($self, $quote) = @_;
    shift @{$self->{tokens}};
    bless [ $self->read_form ], $quote;
}

sub with_meta {
    my ($self, $quote) = @_;
    shift @{$self->{tokens}};

    my $meta = $self->read_form;
    my $form = $self->read_form;

    bless [
        symbol->new('with-meta'),
        $form,
        $meta,
    ], 'list';
}

1;
