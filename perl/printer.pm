package printer;
use feature qw(switch);
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw( _pr_str );

use types qw($nil $true $false);

sub _pr_str {
    my($obj) = @_;
    given (ref $obj) {
        when(/^List/) {
            return '(' . join(' ', map {_pr_str($_)} @$obj) . ')';
        }
        when(/^Vector/) {
            return '[' . join(' ', map {_pr_str($_)} @$obj) . ']';
        }
        when(/^HashMap/) {
            my @elems = ();
            foreach my $key (keys %$obj) {
                push(@elems, _pr_str(String->new($key)));
                push(@elems, _pr_str($obj->{$key}));
            }

            return '{' . join(' ', @elems) . '}';
        }
        when(/^String/) { return '"' . $$obj . '"'; }
        default         { return $$obj; }
    }
}

1;
