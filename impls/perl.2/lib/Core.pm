package Core;

use Mo;

use Types;
use Reader;
use Eval;
use Printer;

sub ns {
    {
        '=' => \&equal_to,
        '>' => \&greater_than,
        '>=' => \&greater_equal,
        '<' => \&less_than,
        '<=' => \&less_equal,
        '+' => \&add,
        '-' => \&subtract,
        '*' => \&multiply,
        '/' => \&divide,

        'atom' => \&atom_,
        'atom?' => \&atom_q,
        'deref' => \&deref,
        'reset!' => \&reset,
        'swap!' => \&swap,

        'list' => \&list_,
        'list?' => \&list_q,
        'cons' => \&cons,
        'concat' => \&concat,

        'count' => \&count,
        'empty?' => \&empty_q,

        'read-string' => \&read_string,
        'slurp' => \&slurp,

        'pr-str' => \&pr_str,
        'str' => \&str,
        'prn' => \&prn,
        'println' => \&println,

        'apply' => \&apply,
    }
}

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

sub atom_ { atom(@_) }
sub atom_q { boolean(ref($_[0]) eq 'atom') }
sub deref { $_[0]->[0] }
sub reset { $_[0]->[0] = $_[1] }
sub swap {
    my ($atom, $fn, @args) = @_;
    $atom->[0] = apply($fn, deref($atom), @args);
}

sub list_ { list([@_]) }
sub list_q { boolean(ref($_[0]) eq 'list') }
sub count { number(ref($_[0]) eq 'nil' ? 0 : scalar @{$_[0]}) }
sub empty_q { boolean(@{$_[0]} == 0) }
sub cons { list([$_[0], @{$_[1]}]) }
sub concat { list([map @$_, @_]) }

sub read_string { Reader::read_str(@_) }
sub slurp {
    my ($file) = @_;
    open my $slurp, '<', "$file" or
        die "Couldn't open '$file' for input";
    local $/;
    string(<$slurp>);
}

sub pr_str { string(join ' ', map Printer::pr_str($_), @_) }
sub str { string(join '', map Printer::pr_str($_, 1), @_) }
sub prn { printf "%s\n", join ' ', map Printer::pr_str($_), @_; nil }
sub println { printf "%s\n", join ' ', map Printer::pr_str($_, 1), @_; nil }

sub apply {
    my ($fn, @args) = @_;
    ref($fn) eq 'CODE' ? $fn->(@args) : Eval::eval($fn->(@args));
}

1;
