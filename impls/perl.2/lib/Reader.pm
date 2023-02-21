use strict; use warnings;
package Reader;

use Types;

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

sub read_str {
    my ($str) = @_;
    my $tokens = tokenize($str);
    my $reader = Reader->new(
        tokens => $tokens,
    );
    $reader->read_form;
}

sub tokenize {
    [ $_[0] =~ /
        (?:
            [\s,] |
            ;.*
        )*
        (
            ~@ |
            [\[\]{}()'`~^@] |
            "(?:
                \\. |
                [^\\"]
            )*"? |
            [^\s\[\]{}('"`,;)]*
        )
    /xog ];
}

sub read_form {
    my ($self) = @_;
    local $_ = $self->{tokens}[0];
    /^\($/ ? $self->read_list('list', ')') :
    /^\[$/ ? $self->read_list('vector', ']') :
    /^\{$/ ? $self->read_hash_map('hash_map', '}') :
    /^'$/ ? $self->read_quote('quote') :
    /^`$/ ? $self->read_quote('quasiquote') :
    /^~$/ ? $self->read_quote('unquote') :
    /^~\@$/ ? $self->read_quote('splice-unquote') :
    /^\@$/ ? $self->read_quote('deref') :
    /^\^$/ ? $self->with_meta :
    $self->read_scalar;
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

sub read_hash_map {
    my ($self, $type, $end) = @_;
    my $tokens = $self->{tokens};
    shift @$tokens;
    my $pairs = [];
    while (@$tokens > 0) {
        if ($tokens->[0] eq $end) {
            shift @$tokens;
            return $type->new($pairs);
        }
        push @$pairs, $self->read_form, $self->read_form;
    }
    die "Reached end of input in 'read_hash_map'";
}

my $string_re = qr/"((?:\\.|[^\\"])*)"/;
my $unescape = {
    'n' => "\n",
    't' => "\t",
    '"' => '"',
    '\\' => "\\",
};
sub read_scalar {
    my ($self) = @_;
    my $scalar = local $_ = shift @{$self->{tokens}};

    if (/^"/) {
        s/^$string_re$/$1/ or
            die "Reached end of input looking for '\"'";
        s/\\([nt\"\\])/$unescape->{$1}/ge;
        return string($_);
    }
    return true if $_ eq 'true';
    return false if $_ eq 'false';
    return nil if $_ eq 'nil';
    return number($_) if /^-?\d+$/;
    return keyword($_) if /^:/;
    return symbol($_);
}

sub read_quote {
    my ($self, $quote) = @_;
    shift @{$self->{tokens}};
    return list([symbol($quote), $self->read_form]);
}

sub with_meta {
    my ($self, $quote) = @_;
    shift @{$self->{tokens}};

    my $meta = $self->read_form;
    my $form = $self->read_form;

    bless [
        symbol('with-meta'),
        $form,
        $meta,
    ], 'list';
}

1;
