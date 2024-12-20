package Env;
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = ();

use Types;

sub new {
    my ( $class, $outer, $binds, $exprs ) = @_;
    my $data = { __outer__ => $outer };
    if ($binds) {
        for my $i ( 0 .. $#{$binds} ) {
            if ( ${ $binds->[$i] } eq q{&} ) {

                # variable length arguments
                $data->{ ${ $binds->[ $i + 1 ] } } =
                  Mal::List->new( [ @{$exprs}[ $i .. $#{$exprs} ] ] );
                last;
            }
            $data->{ ${ $binds->[$i] } } = $exprs->[$i];
        }
    }
    return bless $data => $class;
}

sub get {
    my ( $self, $key ) = @_;
    while ( not $self->{$key} ) {
        $self = $self->{__outer__} // return;
    }
    return $self->{$key};
}

## no critic (NamingConventions::ProhibitAmbiguousNames)
sub set {
    ## use critic
    my ( $self, $key, $value ) = @_;
    $self->{$key} = $value;
    return $value;
}

#my $e1 = Env->new();
#print Dumper($e1);
#
#my $e2 = Env->new();
#$e2->set('abc', 123);
#$e2->set('def', 456);
#print Dumper($e2);
#
#my $e3 = Env->new($e2);
#$e3->set('abc', 789);
#$e3->set('ghi', 1024);
#print Dumper($e3);
#
#print Dumper($e3->get('abc'));
#print Dumper($e3->get('def'));

1;
