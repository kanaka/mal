package Core;

use Mo;

use Types;
use Printer;

sub binds { [qw(
    = > >= < <=
    + - * /
    count empty? list list?
    pr-str str prn println
)] }

sub exprs { [
    \&equal_to, \&greater_than, \&greater_equal, \&less_than, \&less_equal,
    \&add, \&subtract, \&multiply, \&divide,
    \&count, \&is_empty, \&list_, \&is_list,
    \&pr_str, \&str, \&prn, \&println,
] }

sub equal_to {
    my ($x, $y) = @_;
    return false
        unless
            ($x->isa('List') and $y->isa('List')) or
            (ref($x) eq ref($y));
    if ($x->isa('List')) {
        return false unless @$x == @$y;
        for (my $i = 0; $i < @$x; $i++) {
            my $bool = equal_to($x->[$i], $y->[$i]);
            return false if "$bool" eq '0';
        }
        return true;
    }
    boolean($$x eq $$y);
}

sub greater_than { $_[0] > $_[1] }
sub greater_equal { $_[0] >= $_[1] }
sub less_than { $_[0] < $_[1] }
sub less_equal { $_[0] <= $_[1] }
sub add { $_[0] + $_[1] }
sub subtract { $_[0] - $_[1] }
sub multiply { $_[0] * $_[1] }
sub divide { $_[0] / $_[1] }

sub count { number(ref($_[0]) eq 'nil' ? 0 : scalar @{$_[0]}) }
sub is_empty { boolean(@{$_[0]} == 0) }
sub list_ { list([@_]) }
sub is_list { boolean(ref($_[0]) eq 'list') }
sub pr_str { string(join ' ', map Printer::pr_str($_), @_) }
sub str { string(join '', map Printer::pr_str($_, 1), @_) }
sub prn { printf "%s\n", join ' ', map Printer::pr_str($_), @_; nil }
sub println { printf "%s\n", join ' ', map Printer::pr_str($_, 1), @_; nil }

1;
