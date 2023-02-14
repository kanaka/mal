package Env;

use Mo qw< build default xxx >;

use Types;

has outer => ( default => undef, lazy => 0);
has stash => ( default => {}, lazy => 0 );
has binds => [];
has exprs => [];

sub BUILD {
    my ($self) = @_;
    my $binds = [ @{$self->{binds} // []} ];
    my $exprs = $self->{exprs} // [];
    while (@$binds) {
        if ("$binds->[0]" eq '&') {
            shift @$binds;
            $exprs = [list([@$exprs])];
        }
        $self->set(shift(@$binds), shift(@$exprs) // nil);
    }
    delete $self->{binds};
    delete $self->{exprs};
}

sub add {
    my ($self, $ns) = @_;
    my $stash = $self->stash;
    %$stash = (%$stash, %$ns);
    return $self;
}

sub set {
    my ($self, $key, $val) = @_;
    $self->{stash}{$key} = $val;
}

sub find {
    my ($self, $key) = @_;
    while ($self) {
        return $self if defined $self->{stash}{$key};
        $self = $self->{outer};
    }
    return;
}

sub get {
    my ($self, $key) = @_;
    my $env = $self->find($key) or
        die "Symbol '$key' not found in Env";
    $env->{stash}{$key};
}

1;
