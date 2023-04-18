package Interop;
use re '/msx';
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = qw( pl_to_mal );
use Scalar::Util qw(looks_like_number);

use Types qw(nil);

sub pl_to_mal {
    my ($obj) = @_;
    defined $obj or return nil;
    $_ = ref $obj;
    if (/^ARRAY/) {
        return Mal::List->new( [ map { pl_to_mal($_) } @{$obj} ] );
    }
    if (/^HASH/) {
        return Mal::HashMap->new( { map { pl_to_mal($_) } %{$obj} } );
    }
    if ( $_ eq q{} ) {
        if ( looks_like_number $obj ) {
            return Mal::Integer->new($obj);
        }
        return Mal::String->new($obj);
    }
    die 'Failed to convert a perl object to mal.';
}

1;
