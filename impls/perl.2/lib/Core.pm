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
        'assoc' => \&assoc,
        'atom' => \&atom_,
        'atom?' => \&atom_q,
        'concat' => \&concat,
        'cons' => \&cons,
        'contains?' => \&contains_q,
        'count' => \&count,
        'deref' => \&deref,
        'dissoc' => \&dissoc,
        'empty?' => \&empty_q,
        'false?' => \&false_q,
        'first' => \&first,
        'get' => \&get,
        'hash-map' => \&hash_map_,
        'keys' => \&keys,
        'keyword' => \&keyword_,
        'keyword?' => \&keyword_q,
        'list' => \&list_,
        'list?' => \&list_q,
        'map' => \&map_,
        'map?' => \&map_q,
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
        'sequential?' => \&sequential_q,
        'slurp' => \&slurp,
        'str' => \&str,
        'swap!' => \&swap,
        'symbol' => \&symbol_,
        'symbol?' => \&symbol_q,
        'throw' => \&throw,
        'true?' => \&true_q,
        'vals' => \&vals,
        'vec' => \&vec,
        'vector' => \&vector_,
        'vector?' => \&vector_q,
    }
}

sub add { $_[0] + $_[1] }

sub apply {
    my ($fn, @args) = @_;
    push @args, @{pop(@args)};
    ref($fn) eq 'CODE' ? $fn->(@args) : Eval::eval($fn->(@args));
}

sub assoc {
    my ($map, @pairs) = @_;
    for (my $i = 0; $i < @pairs; $i += 2) {
        $pairs[$i] = qq<"$pairs[$i]>
            if $pairs[$i]->isa('string');
    }
    hash_map([%$map, @pairs]);
}

sub atom_ { atom(@_) }

sub atom_q { boolean(ref($_[0]) eq 'atom') }

sub concat { list([map @$_, @_]) }

sub cons { list([$_[0], @{$_[1]}]) }

sub contains_q {
    my ($map, $key) = @_;
    return false unless ref($map) eq 'hash_map';
    $key = qq<"$key> if $key->isa('string');
    boolean(exists $map->{"$key"});
}

sub count { number(ref($_[0]) eq 'nil' ? 0 : scalar @{$_[0]}) }

sub deref { $_[0]->[0] }

sub dissoc {
    my ($map, @keys) = @_;
    @keys = map {
        $_->isa('string') ? qq<"$_> : "$_";
    } @keys;
    $map = { %$map };
    delete $map->{$_} for @keys;
    hash_map([%$map]);
}

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
    if ($x->isa('hash_map')) {
        my @xkeys = sort map "$_", keys %$x;
        my @ykeys = sort map "$_", keys %$y;
        return false unless @xkeys == @ykeys;
        my @xvals = map $x->{$_}, @xkeys;
        my @yvals = map $y->{$_}, @ykeys;
        for (my $i = 0; $i < @xkeys; $i++) {
            return false unless "$xkeys[$i]" eq "$ykeys[$i]";
            my $bool = equal_to($xvals[$i], $yvals[$i]);
            return false if "$bool" eq '0';
        }
        return true;
    }
    boolean($$x eq $$y);
}

sub false_q { boolean(ref($_[0]) eq 'boolean' and not "$_[0]") }

sub first { ref($_[0]) eq 'nil' ? nil : @{$_[0]} ? $_[0]->[0] : nil }

sub get {
    my ($map, $key) = @_;
    return nil unless ref($map) eq 'hash_map';
    $key = qq<"$key> if $key->isa('string');
    $map->{"$key"} // nil;
}

sub greater_equal { $_[0] >= $_[1] }

sub greater_than { $_[0] > $_[1] }

sub hash_map_ { hash_map([@_]) }

sub keys {
    my ($map) = @_;
    my @keys = map {
        s/^"// ? string($_) :
        s/^:// ? keyword($_) :
        symbol("$_");
    } keys %$map;
    list([@keys]);
}

sub keyword_ { keyword($_[0]) }

sub keyword_q { boolean(ref($_[0]) eq 'keyword') }

sub less_equal { $_[0] <= $_[1] }

sub less_than { $_[0] < $_[1] }

sub list_ { list([@_]) }

sub list_q { boolean(ref($_[0]) eq 'list') }

sub map_ { list([ map apply($_[0], $_, []), @{$_[1]} ]) }

sub map_q { boolean(ref($_[0]) eq "hash_map") }

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

sub sequential_q { boolean(ref($_[0]) =~ /^(list|vector)/) }

sub slurp {
    my ($file) = @_;
    open my $slurp, '<', "$file" or
        die "Couldn't open '$file' for input";
    local $/;
    string(<$slurp>);
}

sub str { string(join '', map Printer::pr_str($_, 1), @_) }

sub subtract { $_[0] - $_[1] }

sub symbol_ { symbol($_[0]) }

sub symbol_q { boolean(ref($_[0]) eq 'symbol') }

sub swap {
    my ($atom, $fn, @args) = @_;
    $atom->[0] = apply($fn, deref($atom), \@args);
}

sub throw { die $_[0] }

sub true_q { boolean(ref($_[0]) eq 'boolean' and "$_[0]") }

sub vals { list([ values %{$_[0]} ]) }

sub vec { vector([@{$_[0]}]) }

sub vector_ { vector([@_]) }

sub vector_q { boolean(ref($_[0]) eq "vector") }

1;
