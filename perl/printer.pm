package printer;
use strict;
use warnings FATAL => qw(all);
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw( _pr_str );

use types qw($nil $true $false);

use Data::Dumper;

sub _pr_str {
    my($obj, $print_readably) = @_;
    my($_r) = (defined $print_readably) ? $print_readably : 1;
    given (ref $obj) {
        when(/^List/) {
            return '(' . join(' ', map {_pr_str($_, $_r)} @{$obj->{val}}) . ')';
        }
        when(/^Vector/) {
            return '[' . join(' ', map {_pr_str($_, $_r)} @{$obj->{val}}) . ']';
        }
        when(/^HashMap/) {
            my @elems = ();
            foreach my $key (keys $obj->{val}) {
                push(@elems, _pr_str(String->new($key), $_r));
                push(@elems, _pr_str($obj->{val}->{$key}, $_r));
            }

            return '{' . join(' ', @elems) . '}';
        }
        when(/^String/) {
            if ($_r) {
                my $str = $$obj;
                $str =~ s/\\/\\\\/g;
                $str =~ s/"/\\"/g;
                $str =~ s/\n/\\n/g;
                return '"' . $str . '"';
            } else {
                return $$obj;
            }
        }
        when(/^Function/) {
            return '<fn* ' . _pr_str($obj->{params}) .
                   ' ' . _pr_str($obj->{ast}) . '>';
        }
        when(/^Atom/) {
            return '(atom ' . _pr_str($obj->{val}) . ")";
        }
        when(/^CODE/)   { return '<builtin_fn* ' . $obj . '>'; }
        default         { return $$obj; }
    }
}

1;
