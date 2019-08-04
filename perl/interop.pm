package interop;
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = qw( pl_to_mal );
use List::Util qw(pairmap);
use Scalar::Util qw(looks_like_number);

use types qw($nil);

sub pl_to_mal {
    my($obj) = @_;
    for (ref $obj) {
        if (/^ARRAY/) {
            my @arr = map {pl_to_mal($_)} @$obj;
            return Mal::List->new(\@arr);
        } elsif (/^HASH/) {
            my %hsh = map { pl_to_mal($_) } %$obj;
            return Mal::HashMap->new(\%hsh)
        } else {
	    if (!defined($obj)) {
		return $nil;
            } elsif (looks_like_number($obj)) {
                return Mal::Integer->new($obj);
            } else {
                return Mal::String->new($obj);
            }
        }
    }
}

1;
