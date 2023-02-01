use v5.18;

package Printer;

use Mo;

sub pr_str {
    my ($o) = @_;

    if (ref $o) {
        return '(' . join(' ', map pr_str($_), @$o) . ')';
    }
    else {
        return $o;
    }
}

1;
