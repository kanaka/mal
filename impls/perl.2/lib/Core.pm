use strict; use warnings;
package Core;

use Types;
use Reader;
use Eval;
use Printer;

sub ns {
    {
        '*' => \&multiply,
        '+' => \&add,
        '-' => \&subtract,
        '/' => \&divide,
        '<' => \&less_than,
        '<=' => \&less_equal,
        '=' => \&equal_to,
        '>' => \&greater_than,
        '>=' => \&greater_equal,
        'apply' => \&apply,
        'atom' => \&atom_,
        'atom?' => \&atom_q,
        'concat' => \&concat,
        'cons' => \&cons,
        'count' => \&count,
        'deref' => \&deref,
        'empty?' => \&empty_q,
        'false?' => \&false_q,
        'first' => \&first,
        'list' => \&list_,
        'list?' => \&list_q,
        'map' => \&map_,
        'nil?' => \&nil_q,
        'nth' => \&nth,
        'first' => \&first,
        'rest' => \&rest,

        'count' => \&count,
        'empty?' => \&empty_q,

        'read-string' => \&read_string,
        'slurp' => \&slurp,

        'pr-str' => \&pr_str,
        'println' => \&println,
        'prn' => \&prn,
        'read-string' => \&read_string,
        'reset!' => \&reset,
        'rest' => \&rest,
        'slurp' => \&slurp,
        'str' => \&str,
        'swap!' => \&swap,
        'symbol?' => \&symbol_q,
        'throw' => \&throw,
        'true?' => \&true_q,
        'vec' => \&vec,
    }
}

sub add { $_[0] + $_[1] }

sub apply {
    my ($fn, @args) = @_;
    push @args, @{pop(@args)};
    ref($fn) eq 'CODE' ? $fn->(@args) : Eval::eval($fn->(@args));
}

sub atom_ { atom(@_) }

sub atom_q { boolean(ref($_[0]) eq 'atom') }

sub concat { list([map @$_, @_]) }

sub cons { list([$_[0], @{$_[1]}]) }

sub count { number(ref($_[0]) eq 'nil' ? 0 : scalar @{$_[0]}) }

sub deref { $_[0]->[0] }

sub divide { $_[0] / $_[1] }

sub empty_q { boolean(@{$_[0]} == 0) }

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

sub false_q { boolean(ref($_[0]) eq 'boolean' and not "$_[0]") }

sub first { ref($_[0]) eq 'nil' ? nil : @{$_[0]} ? $_[0]->[0] : nil }

sub greater_equal { $_[0] >= $_[1] }

sub greater_than { $_[0] > $_[1] }

sub less_equal { $_[0] <= $_[1] }

sub less_than { $_[0] < $_[1] }

sub list_ { list([@_]) }

sub list_q { boolean(ref($_[0]) eq 'list') }

sub map_ { list([ map apply($_[0], $_, []), @{$_[1]} ]) }

sub multiply { $_[0] * $_[1] }

sub nil_q { boolean(ref($_[0]) eq 'nil') }

sub nth {
    my ($list, $index) = @_;
    die "Index '$index' out of range" if $index >= @$list;
    $list->[$index];
}

sub pr_str { string(join ' ', map Printer::pr_str($_), @_) }

sub println { printf "%s\n", join ' ', map Printer::pr_str($_, 1), @_; nil }

sub prn { printf "%s\n", join ' ', map Printer::pr_str($_), @_; nil }

sub read_string { Reader::read_str(@_) }

sub reset { $_[0]->[0] = $_[1] }

sub rest {
    my ($list) = @_;
    return list([]) if $list->isa('nil') or not @$list;
    shift @$list;
    list([@$list]);
}

sub slurp {
    my ($file) = @_;
    open my $slurp, '<', "$file" or
        die "Couldn't open '$file' for input";
    local $/;
    string(<$slurp>);
}

sub str { string(join '', map Printer::pr_str($_, 1), @_) }

sub subtract { $_[0] - $_[1] }

sub symbol_q { boolean(ref($_[0]) eq 'symbol') }

sub swap {
    my ($atom, $fn, @args) = @_;
    $atom->[0] = apply($fn, deref($atom), \@args);
}

sub throw {
    die "$_[0]\n";
}

sub true_q { boolean(ref($_[0]) eq 'boolean' and "$_[0]") }

sub vec { vector([@{$_[0]}]) }

1;
