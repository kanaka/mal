package printer;
use strict;
use warnings;
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw( _pr_str );

use types qw($nil $true $false);

sub _pr_str {
    my($obj, $print_readably) = @_;
    my($_r) = (defined $print_readably) ? $print_readably : 1;
    given (ref $obj) {
        when(/^List/) {
            return '(' . join(' ', map {_pr_str($_, $_r)} @$obj) . ')';
        }
        when(/^Vector/) {
            return '[' . join(' ', map {_pr_str($_, $_r)} @$obj) . ']';
        }
        when(/^HashMap/) {
            my @elems = ();
            foreach my $key (keys %$obj) {
                push(@elems, _pr_str(String->new($key), $_r));
                push(@elems, _pr_str($obj->{$key}, $_r));
            }

            return '{' . join(' ', @elems) . '}';
        }
        when(/^String/) {
            if ($_r) {
                my $str = $$obj;
                $str =~ s/\\/\\\\/g;
                $str =~ s/"/\\"/g;
                $str =~ s/\n/\\n"/g;
                return '"' . $str . '"';
            } else {
                return $$obj;
            }
        }
        when(/^CODE/)   { return '<builtin_fn* ' . $obj . '>'; }
        default         { return $$obj; }
    }
}

1;
