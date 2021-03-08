package core;
use strict;
use warnings;

use Data::Dumper;
use Hash::Util qw(fieldhash);
use List::Util qw(pairmap);
use Time::HiRes qw(time);

use readline;
use types qw(_equal_Q thaw_key $nil $true $false);
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
    return Mal::HashMap->new( { %$src_hsh, @_ } );
}

sub dissoc {
    my $new_hsh = { %{shift @_} };
    delete @{$new_hsh}{@_};
    return Mal::HashMap->new($new_hsh);
}


sub get {
    my ($hsh, $key) = @_;
    return $hsh->{$key} // $nil;
}

sub contains_Q {
    my ($hsh, $key) = @_;
    return (exists $hsh->{$key}) ? $true : $false;
}

sub mal_keys {
    my @ks = map { thaw_key($_) } keys %{$_[0]};
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
    push @_, @{pop @_};
    goto &$f;
}

sub mal_map {
    my $f = shift;
    my @arr = map { &$f($_) } @{$_[0]};
    return Mal::List->new(\@arr);
}

sub conj {
    my $seq = shift;
    my $new_seq = $seq->clone;
    if ($new_seq->isa('Mal::List')) {
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
    } elsif ($arg->isa('Mal::List')) {
        return $nil unless @$arg;
        return $arg;
    } elsif ($arg->isa('Mal::Vector')) {
        return $nil unless @$arg;
        return Mal::List->new([@$arg]);
    } elsif ($arg->isa('Mal::String')) {
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
    my $new_obj = $_[0]->clone;
    $meta{$new_obj} = $_[1];
    return $new_obj;
}


# Atom functions
sub swap_BANG {
    my ($atm,$f,@args) = @_;
    return $$atm = &$f($$atm, @args);
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
    'number?'     => sub { $_[0]->isa('Mal::Integer') ? $true : $false },
    'symbol'      => sub { Mal::Symbol->new(${$_[0]}) },
    'symbol?'     => sub { $_[0]->isa('Mal::Symbol') ? $true : $false },
    'string?'     => sub { $_[0]->isa('Mal::String') ? $true : $false },
    'keyword'     => sub { Mal::Keyword->new(${$_[0]}) },
    'keyword?'    => sub { $_[0]->isa('Mal::Keyword') ? $true : $false },
    'fn?'         => sub { $_[0]->isa('Mal::Function') ? $true : $false },
    'macro?'      => sub { $_[0]->isa('Mal::Macro') ? $true : $false },

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
    'list?'       => sub { $_[0]->isa('Mal::List') ? $true : $false },
    'vector'      => sub { Mal::Vector->new(\@_) },
    'vector?'     => sub { $_[0]->isa('Mal::Vector') ? $true : $false },
    'hash-map'    => sub { Mal::HashMap->new(\@_) },
    'map?'        => sub { $_[0]->isa('Mal::HashMap') ? $true : $false },
    'assoc'       => \&assoc,
    'dissoc'      => \&dissoc,
    'get'         => \&get,
    'contains?'   => \&contains_Q,
    'keys'        => \&mal_keys,
    'vals'        => \&mal_vals,

    'sequential?' => sub { $_[0]->isa('Mal::Sequence') ? $true : $false },
    'nth'         => sub { nth($_[0], ${$_[1]}) },
    'first'       => \&first,
    'rest'        => sub { $_[0]->rest() },
    'cons'        => \&cons,
    'concat'      => sub { Mal::List->new([map @$_, @_]) },
    'vec'         => sub { Mal::Vector->new([@{$_[0]}]) },
    'empty?'      => sub { @{$_[0]} ? $false : $true },
    'count'       => sub { Mal::Integer->new(scalar(@{$_[0]})) },
    'apply'       => \&apply,
    'map'         => \&mal_map,
    'conj'        => \&conj,
    'seq'         => \&seq,

    'with-meta'   => \&with_meta,
    'meta'        => sub { $meta{$_[0]} // $nil },
    'atom'        => sub { Mal::Atom->new($_[0]) },
    'atom?'       => sub { $_[0]->isa('Mal::Atom') ? $true : $false },
    'deref'       => sub { ${$_[0]} },
    'reset!'      => sub { ${$_[0]} = $_[1] },
    'swap!'       => \&swap_BANG,

    'pl*'         => \&pl_STAR,
);

foreach my $f (values %core::ns) {
    $f = Mal::Function->new($f);
}

1;
