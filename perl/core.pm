package core;
use strict;
use warnings;

use Data::Dumper;
use Hash::Util qw(fieldhash);
use List::Util qw(pairmap);
use Time::HiRes qw(time);

use readline;
use types qw(_sequential_Q _equal_Q _clone $nil $true $false
             _number_Q _symbol _symbol_Q _string_Q _keyword _keyword_Q _list_Q _vector_Q _sub_Q _function_Q
             _hash_map _hash_map_Q _atom_Q);
use reader qw(read_str);
use printer qw(_pr_str);
use interop qw(pl_to_mal);

# String functions

sub pr_str {
    return Mal::String->new(join(" ", map {_pr_str($_, 1)} @_));
}

sub str {
    return Mal::String->new(join("", map {_pr_str($_, 0)} @_));
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
    return defined $line ? Mal::String->new($line) : $nil;
}

sub slurp {
    use autodie;
    open(my $fh, '<', ${$_[0]});
    my $data = do { local $/; <$fh> };
    Mal::String->new($data)
}

# Hash Map functions

sub assoc {
    my $src_hsh = shift;
    return Mal::HashMap->new( { %$src_hsh, pairmap { $$a => $b } @_ } );
}

sub dissoc {
    my $new_hsh = { %{shift @_} };
    delete @{$new_hsh}{map $$_, @_};
    return Mal::HashMap->new($new_hsh);
}


sub get {
    my ($hsh, $key) = @_;
    return $hsh->{$$key} // $nil;
}

sub contains_Q {
    my ($hsh, $key) = @_;
    return (exists $hsh->{$$key}) ? $true : $false;
}

sub mal_keys {
    my @ks = map { Mal::String->new($_) } keys %{$_[0]};
    return Mal::List->new(\@ks);
}

sub mal_vals {
    my @vs = values %{$_[0]};
    return Mal::List->new(\@vs);
}


# Sequence functions

sub cons {
    my ($a, $b) = @_;
    Mal::List->new([$a, @$b]);
}

sub nth {
    my ($seq,$i) = @_;
    return $seq->[$i] // die "nth: index out of bounds";
}

sub first {
    my ($seq) = @_;
    return $seq->[0] // $nil;
}

sub apply {
    my $f = shift;
    my $more_args = pop;
    return &$f(@_, @$more_args);
}

sub mal_map {
    my $f = shift;
    my @arr = map { &$f($_) } @{$_[0]};
    return Mal::List->new(\@arr);
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
    if ($arg eq $nil) {
        return $nil;
    } elsif (_list_Q($arg)) {
        return $nil unless @$arg;
        return $arg;
    } elsif (_vector_Q($arg)) {
        return $nil unless @$arg;
        return Mal::List->new([@$arg]);
    } elsif (_string_Q($arg)) {
        return $nil if length($$arg) == 0;
        my @chars = map { Mal::String->new($_) } split(//, $$arg);
        return Mal::List->new(\@chars);
    } else {
        die "seq requires list or vector or string or nil";
    }
}

fieldhash my %meta;

# Metadata functions
sub with_meta {
    my $new_obj = _clone($_[0]);
    $meta{$new_obj} = $_[1];
    return $new_obj;
}


# Atom functions
sub swap_BANG {
    my ($atm,$f,@args) = @_;
    unshift @args, $$atm;
    return $$atm = &$f(@args);
}


# Interop

sub pl_STAR {
    my $result = eval(${$_[0]});
    die $@ if $@;
    return pl_to_mal($result);
}



%core::ns = (
    '='           => sub { _equal_Q($_[0], $_[1]) ? $true : $false },
    'throw'       => sub { die $_[0] },
    'nil?'        => sub { $_[0] eq $nil ? $true : $false },
    'true?'       => sub { $_[0] eq $true ? $true : $false },
    'false?'      => sub { $_[0] eq $false ? $true : $false },
    'number?'     => sub { _number_Q($_[0]) ? $true : $false },
    'symbol'      => sub { Mal::Symbol->new(${$_[0]}) },
    'symbol?'     => sub { _symbol_Q($_[0]) ? $true : $false },
    'string?'     => sub { _string_Q($_[0]) ? $true : $false },
    'keyword'     => sub { _keyword(${$_[0]}) },
    'keyword?'    => sub { _keyword_Q($_[0]) ? $true : $false },
    'fn?'         => sub { (_sub_Q($_[0]) || (_function_Q($_[0]) && !$_[0]->{ismacro})) ? $true : $false },
    'macro?'      => sub { (_function_Q($_[0]) && $_[0]->{ismacro}) ? $true : $false },

    'pr-str'      => \&pr_str,
    'str'         => \&str,
    'prn'         => \&prn,
    'println'     => \&println,
    'readline'    => \&mal_readline,
    'read-string' => sub { read_str(${$_[0]}) },
    'slurp'       => \&slurp,
    '<'           => sub { ${$_[0]} <  ${$_[1]} ? $true : $false },
    '<='          => sub { ${$_[0]} <= ${$_[1]} ? $true : $false },
    '>'           => sub { ${$_[0]} >  ${$_[1]} ? $true : $false },
    '>='          => sub { ${$_[0]} >= ${$_[1]} ? $true : $false },
    '+'           => sub { Mal::Integer->new(${$_[0]} + ${$_[1]}) },
    '-'           => sub { Mal::Integer->new(${$_[0]} - ${$_[1]}) },
    '*'           => sub { Mal::Integer->new(${$_[0]} * ${$_[1]}) },
    '/'           => sub { Mal::Integer->new(${$_[0]} / ${$_[1]}) },
    'time-ms'     => sub { Mal::Integer->new(int(time()*1000)) },

    'list'        => sub { Mal::List->new(\@_) },
    'list?'       => sub { _list_Q($_[0]) ? $true : $false },
    'vector'      => sub { Mal::Vector->new(\@_) },
    'vector?'     => sub { _vector_Q($_[0]) ? $true : $false },
    'hash-map'    => \&_hash_map,
    'map?'        => sub { _hash_map_Q($_[0]) ? $true : $false },
    'assoc'       => \&assoc,
    'dissoc'      => \&dissoc,
    'get'         => \&get,
    'contains?'   => \&contains_Q,
    'keys'        => \&mal_keys,
    'vals'        => \&mal_vals,

    'sequential?' => sub { _sequential_Q($_[0]) ? $true : $false },
    'nth'         => sub { nth($_[0], ${$_[1]}) },
    'first'       => \&first,
    'rest'        => sub { $_[0]->rest() },
    'cons'        => \&cons,
    'concat'      => sub { Mal::List->new([map @$_, @_]) },
    'empty?'      => sub { @{$_[0]} ? $false : $true },
    'count'       => sub { Mal::Integer->new(scalar(@{$_[0]})) },
    'apply'       => \&apply,
    'map'         => \&mal_map,
    'conj'        => \&conj,
    'seq'         => \&seq,

    'with-meta'   => \&with_meta,
    'meta'        => sub { $meta{$_[0]} // $nil },
    'atom'        => sub { Mal::Atom->new($_[0]) },
    'atom?'       => sub { _atom_Q($_[0]) ? $true : $false },
    'deref'       => sub { ${$_[0]} },
    'reset!'      => sub { ${$_[0]} = $_[1] },
    'swap!'       => \&swap_BANG,

    'pl*'         => \&pl_STAR,
);

foreach my $f (values %core::ns) {
    bless $f, 'Mal::CoreFunction';
}

1;
