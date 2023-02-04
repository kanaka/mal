package Env;

use Types;

use Mo qw< default >;

has outer => ( default => undef, lazy => 0);
has ns => ( default => {}, lazy => 0 );

sub set {
    my ($self, $key, $val) = @_;
    $self->{ns}{$key} = $val;
}

sub find {
    my ($self, $key) = @_;
    while ($self) {
        if (defined $self->{ns}{$key}) {
            return $self;
        }
        $self = $self->{outer};
    }
    die "Symbol '$key' not found in Env";
}

sub get {
    my ($self, $key) = @_;
    $self->find($key)->{ns}{$key};
}

1;
