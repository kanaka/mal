package interop;
use strict;
use warnings FATAL => qw(all);
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw( pl_to_mal );
use Scalar::Util qw(looks_like_number);

use types;

sub pl_to_mal {
    my($obj) = @_;
    given (ref $obj) {
        when(/^ARRAY/) {
            my @arr = map {pl_to_mal($_)} @$obj;
            return List->new(\@arr);
        }
        when(/^HASH/) {
            my $hsh = {};
            foreach my $key (keys %$obj) {
                $hsh->{$key} = pl_to_mal($obj->{$key});
            }
            return HashMap->new($hsh)
        }
        default {
            if (looks_like_number($obj)) {
                return Integer->new($obj);
            } else {
                return String->new($obj);
            }
        }
    }
}

1;
