package interop;
use strict;
use warnings FATAL => qw(all);
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw( pl_to_mal );
use Scalar::Util qw(looks_like_number);

use types qw($nil);

sub pl_to_mal {
    my($obj) = @_;
    given (ref $obj) {
        when(/^ARRAY/) {
            my @arr = map {pl_to_mal($_)} @$obj;
            return Mal::List->new(\@arr);
        }
        when(/^HASH/) {
            my $hsh = {};
            foreach my $key (keys %$obj) {
                $hsh->{$key} = pl_to_mal($obj->{$key});
            }
            return Mal::HashMap->new($hsh)
        }
        default {
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
