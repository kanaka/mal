package core;
use strict;
use warnings FATAL => qw(all);
use Time::HiRes qw(time);

use readline;
use types qw(_sequential_Q _equal_Q _clone $nil $true $false
             _nil_Q _true_Q _false_Q
             _number_Q _symbol _symbol_Q _string_Q _keyword _keyword_Q _list_Q _vector_Q _sub_Q _function_Q
             _hash_map _hash_map_Q _assoc_BANG _atom_Q);
use reader qw(read_str);
use printer qw(_pr_str);

use Data::Dumper;

# String functions

sub pr_str {
    return String->new(join(" ", map {_pr_str($_, 1)} @_));
}

sub str {
    return String->new(join("", map {_pr_str($_, 0)} @_));
}

sub prn {
    print join(" ", map {_pr_str($_, 1)} @_) . "\n";
    return $nil
}

sub println {
    print join(" ", map {_pr_str($_, 0)} @_) . "\n";
    return $nil
}

sub mal_readline {
    my $line = readline::mal_readline(${$_[0]});
    return defined $line ? String->new($line) : $nil;
}

sub slurp {
    my $fname = ${$_[0]}; 
    open(my $fh, '<', $fname) or die "error opening '$fname'";
    my $data = do { local $/; <$fh> };
    String->new($data)
}

# Hash Map functions

sub assoc {
    my $src_hsh = shift;
    my $new_hsh = { %$src_hsh };
    return _assoc_BANG($new_hsh, @_);
}

sub dissoc {
    my $new_hsh = { %{shift @_} };
    delete @{$new_hsh}{map $$_, @_};
    return HashMap->new($new_hsh);
}


sub get {
    my ($hsh, $key) = @_;
    return $hsh->{$$key} || $nil;
}

sub contains_Q {
    my ($hsh, $key) = @_;
    return (exists $hsh->{$$key}) ? $true : $false;
}

sub mal_keys {
    my @ks = map { String->new($_) } keys %{$_[0]};
    return List->new(\@ks);
}

sub mal_vals {
    my @vs = values %{$_[0]};
    return List->new(\@vs);
}


# Sequence functions

sub cons {
    my ($a, $b) = @_;
    List->new([$a, @$b]);
}

sub concat {
    List->new([map @$_, @_]);
}

sub nth {
    my ($seq,$i) = @_;
    return $seq->[$i] || die "nth: index out of bounds";
}

sub first {
    my ($seq) = @_;
    return $seq->[0] || $nil;
}

sub rest { return $_[0]->rest(); }

sub count {
    return Integer->new(scalar(@{$_[0]}))
}

sub apply {
    my $f = shift;
    my $more_args = pop;
    return &$f(@_, @$more_args);
}

sub mal_map {
    my $f = shift;
    my @arr = map { &$f($_) } @{$_[0]};
    return List->new(\@arr);
}

sub conj {
    my $seq = shift;
    my $new_seq = _clone($seq);
    if (_list_Q($new_seq)) {
        unshift @$new_seq, reverse @_;
    } else {
        push @$new_seq, @_;
    }
    return $new_seq;
}

sub seq {
    my ($arg) = @_;
    if (_nil_Q($arg)) {
        return $nil;
    } elsif (_list_Q($arg)) {
        return $nil unless @$arg;
        return $arg;
    } elsif (_vector_Q($arg)) {
        return $nil unless @$arg;
        return List->new([@$arg]);
    } elsif (_string_Q($arg)) {
        return $nil if length($$arg) == 0;
        my @chars = map { String->new($_) } split(//, $$arg);
        return List->new(\@chars);
    } else {
        die "seq requires list or vector or string or nil";
    }
}

# Metadata functions
sub with_meta {
    no overloading '%{}';
    my $new_obj = _clone($_[0]);
    $new_obj->{meta} = $_[1];
    return $new_obj;
}

sub meta {
    return $_[0]->meta;
}


# Atom functions
sub swap_BANG {
    my ($atm,$f,@args) = @_;
    unshift @args, $$atm;
    return $$atm = &$f(@args);
}



%core::ns = (
    '=' =>  sub { _equal_Q($_[0], $_[1]) ? $true : $false },
    'throw' => sub { die $_[0] },
    'nil?' => sub { _nil_Q($_[0]) ? $true : $false },
    'true?' => sub { _true_Q($_[0]) ? $true : $false },
    'false?' => sub { _false_Q($_[0]) ? $true : $false },
    'number?' => sub { _number_Q($_[0]) ? $true : $false },
    'symbol'  => sub { Symbol->new(${$_[0]}) },
    'symbol?' => sub { _symbol_Q($_[0]) ? $true : $false },
    'string?' => sub { _string_Q($_[0]) ? $true : $false },
    'keyword'  => sub { _keyword(${$_[0]}) },
    'keyword?' => sub { _keyword_Q($_[0]) ? $true : $false },
    'fn?' => sub { (_sub_Q($_[0]) || (_function_Q($_[0]) && !$_[0]->{ismacro})) ? $true : $false },
    'macro?' => sub { (_function_Q($_[0]) && $_[0]->{ismacro}) ? $true : $false },

    'pr-str' =>  \&pr_str,
    'str' =>     \&str,
    'prn' =>     \&prn,
    'println' => \&println,
    'readline' =>    \&mal_readline,
    'read-string' => sub { read_str(${$_[0]}) },
    'slurp' =>       \&slurp,
    '<' =>  sub { ${$_[0]} <  ${$_[1]} ? $true : $false },
    '<=' => sub { ${$_[0]} <= ${$_[1]} ? $true : $false },
    '>' =>  sub { ${$_[0]} >  ${$_[1]} ? $true : $false },
    '>=' => sub { ${$_[0]} >= ${$_[1]} ? $true : $false },
    '+' =>  sub { Integer->new(${$_[0]} + ${$_[1]}) },
    '-' =>  sub { Integer->new(${$_[0]} - ${$_[1]}) },
    '*' =>  sub { Integer->new(${$_[0]} * ${$_[1]}) },
    '/' =>  sub { Integer->new(${$_[0]} / ${$_[1]}) },
    'time-ms' => sub { Integer->new(int(time()*1000)) },

    'list'  => sub { List->new(\@_) },
    'list?' => sub { _list_Q($_[0]) ? $true : $false },
    'vector'  => sub { Vector->new(\@_) },
    'vector?' => sub { _vector_Q($_[0]) ? $true : $false },
    'hash-map' => \&_hash_map,
    'map?' => sub { _hash_map_Q($_[0]) ? $true : $false },
    'assoc' => \&assoc,
    'dissoc' => \&dissoc,
    'get' => \&get,
    'contains?' => \&contains_Q,
    'keys' => \&mal_keys,
    'vals' => \&mal_vals,

    'sequential?' => sub { _sequential_Q($_[0]) ? $true : $false },
    'nth' => sub { nth($_[0], ${$_[1]}) },
    'first' => \&first,
    'rest' => \&rest,
    'cons' => \&cons,
    'concat' => \&concat,
    'empty?' => sub { @{$_[0]} ? $false : $true },
    'count' => \&count,
    'apply' => \&apply,
    'map' => \&mal_map,
    'conj' => \&conj,
    'seq' => \&seq,

    'with-meta' => \&with_meta,
    'meta' => \&meta,
    'atom' => sub { Atom->new($_[0]) },
    'atom?' => sub { _atom_Q($_[0]) ? $true : $false },
    'deref' => sub { ${$_[0]} },
    'reset!' => sub { ${$_[0]} = $_[1] },
    'swap!' => \&swap_BANG,
);

foreach my $f (values %core::ns) {
    bless $f, 'CoreFunction';
}

1;
