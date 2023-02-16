use strict; use warnings;
package Env;

use Types;

sub new {
    my $class = shift;
    my $self = bless {
        outer => undef,
        stash => {},
        @_
    }, $class;
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
    return $self;
}

sub add {
    my ($self, $ns) = @_;
    my $stash = $self->{stash};
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
        die "'$key' not found\n";
    $env->{stash}{$key};
}

1;
