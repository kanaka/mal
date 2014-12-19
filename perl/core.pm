package core;
use strict;
use warnings FATAL => qw(all);
use Exporter 'import';
our @EXPORT_OK = qw($core_ns);
use Time::HiRes qw(time);

use readline;
use types qw(_sequential_Q _equal_Q _clone $nil $true $false
             _nil_Q _true_Q _false_Q
             _symbol _symbol_Q _keyword _keyword_Q _list_Q _vector_Q
             _hash_map _hash_map_Q _assoc_BANG _dissoc_BANG _atom_Q);
use reader qw(read_str);
use printer qw(_pr_str);

use Data::Dumper;

# String functions

sub pr_str {
    return String->new(join(" ", map {_pr_str($_, 1)} @{$_[0]->{val}}));
}

sub str {
    return String->new(join("", map {_pr_str($_, 0)} @{$_[0]->{val}}));
}

sub prn {
    print join(" ", map {_pr_str($_, 1)} @{$_[0]->{val}}) . "\n";
    return $nil
}

sub println {
    print join(" ", map {_pr_str($_, 0)} @{$_[0]->{val}}) . "\n";
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
    my $new_hsh = { %{$src_hsh->{val}} };
    return _assoc_BANG($new_hsh, @_);
}

sub dissoc {
    my $src_hsh = shift;
    my $new_hsh = { %{$src_hsh->{val}} };
    return _dissoc_BANG($new_hsh, @_);
}


sub get {
    my ($hsh, $key) = @_;
    return $nil if $hsh eq $nil;
    return exists $hsh->{val}->{$$key} ? $hsh->{val}->{$$key} : $nil;
}

sub contains_Q {
    my ($hsh, $key) = @_;
    return $nil if $hsh eq $false;
    return (exists $hsh->{val}->{$$key}) ? $true : $false;
}

sub mal_keys {
    my @ks = map { String->new($_) } keys %{$_[0]->{val}};
    return List->new(\@ks);
}

sub mal_vals {
    my @vs = values %{$_[0]->{val}};
    return List->new(\@vs);
}


# Sequence functions

sub cons {
    my ($a, $b) = @_;
    my @new_arr = @{[$a]};
    push @new_arr, @{$b->{val}};
    List->new(\@new_arr);
}

sub concat {
    if (scalar(@_) == 0) { return List->new([]); }
    my ($a) = shift;
    my @new_arr = @{$a->{val}};
    map { push @new_arr, @{$_->{val}} } @_;
    List->new(\@new_arr);
}

sub nth {
    my ($seq,$i) = @_;
    if (@{$seq->{val}} > $i) {
        return scalar($seq->nth($i));
    } else {
        die "nth: index out of bounds";
    }
}

sub first { my ($seq) = @_; return scalar(@{$seq->{val}}) > 0 ? $seq->nth(0) : $nil; }

sub rest { return $_[0]->rest(); }

sub count {
    if (_nil_Q($_[0])) {
        return Integer->new(0);
    } else {
        return Integer->new(scalar(@{$_[0]->{val}}))
    }
}

sub apply {
    my @all_args = @{$_[0]->{val}};
    my $f = $all_args[0];
    my @apply_args = @all_args[1..$#all_args];
    my @args = @apply_args[0..$#apply_args-1];
    push @args, @{$apply_args[$#apply_args]->{val}};
    if ((ref $f) =~ /^Function/) {
        return $f->apply(List->new(\@args));
    } else {
        return &{ $f }(List->new(\@args));
    }
}

sub mal_map {
    my $f = shift;
    my @arr;
    if ((ref $f) =~ /^Function/) {
        @arr = map { $f->apply(List->new([$_])) } @{$_[0]->{val}};
    } else {
        @arr = map { &{ $f}(List->new([$_])) } @{$_[0]->{val}};
    }
    return List->new(\@arr);
}


# Metadata functions
sub with_meta {
    my $new_obj = _clone($_[0]);
    $new_obj->{meta} = $_[1];
    return $new_obj;
}

sub meta {
    if ((ref $_[0]) && !((ref $_[0]) =~ /^CODE/)) {
        return $_[0]->{meta};
    } else {
        return $nil;
    }
}


# Atom functions
sub swap_BANG {
    my ($atm,$f,@args) = @_;
    unshift @args, $atm->{val};
    if ((ref $f) =~ /^Function/) {
        return $atm->{val} = $f->apply(List->new(\@args));
    } else {
        return $atm->{val} = &{ $f }(List->new(\@args));
    }
}



our $core_ns = {
    '=' =>  sub { _equal_Q($_[0]->nth(0), $_[0]->nth(1)) ? $true : $false },
    'throw' => sub { die $_[0]->nth(0) },
    'nil?' => sub { _nil_Q($_[0]->nth(0)) ? $true : $false },
    'true?' => sub { _true_Q($_[0]->nth(0)) ? $true : $false },
    'false?' => sub { _false_Q($_[0]->nth(0)) ? $true : $false },
    'symbol'  => sub { Symbol->new(${$_[0]->nth(0)}) },
    'symbol?' => sub { _symbol_Q($_[0]->nth(0)) ? $true : $false },
    'keyword'  => sub { _keyword(${$_[0]->nth(0)}) },
    'keyword?' => sub { _keyword_Q($_[0]->nth(0)) ? $true : $false },

    'pr-str' =>  sub { pr_str($_[0]) },
    'str' =>     sub { str($_[0]) },
    'prn' =>     sub { prn($_[0]) },
    'println' => sub { println($_[0]) },
    'readline' =>    sub { mal_readline($_[0]->nth(0)) },
    'read-string' => sub { read_str(${$_[0]->nth(0)}) },
    'slurp' =>       sub { slurp($_[0]->nth(0)) },
    '<' =>  sub { ${$_[0]->nth(0)} <  ${$_[0]->nth(1)} ? $true : $false },
    '<=' => sub { ${$_[0]->nth(0)} <= ${$_[0]->nth(1)} ? $true : $false },
    '>' =>  sub { ${$_[0]->nth(0)} >  ${$_[0]->nth(1)} ? $true : $false },
    '>=' => sub { ${$_[0]->nth(0)} >= ${$_[0]->nth(1)} ? $true : $false },
    '+' =>  sub { Integer->new(${$_[0]->nth(0)} + ${$_[0]->nth(1)}) },
    '-' =>  sub { Integer->new(${$_[0]->nth(0)} - ${$_[0]->nth(1)}) },
    '*' =>  sub { Integer->new(${$_[0]->nth(0)} * ${$_[0]->nth(1)}) },
    '/' =>  sub { Integer->new(${$_[0]->nth(0)} / ${$_[0]->nth(1)}) },
    'time-ms' => sub { Integer->new(int(time()*1000)) },

    'list'  => sub { List->new($_[0]->{val}) },
    'list?' => sub { _list_Q($_[0]->nth(0)) ? $true : $false },
    'vector'  => sub { Vector->new($_[0]->{val}) },
    'vector?' => sub { _vector_Q($_[0]->nth(0)) ? $true : $false },
    'hash-map' => sub { _hash_map(@{$_[0]->{val}}) },
    'map?' => sub { _hash_map_Q($_[0]->nth(0)) ? $true : $false },
    'assoc' => sub { assoc(@{$_[0]->{val}}) },
    'dissoc' => sub { dissoc(@{$_[0]->{val}}) },
    'get' => sub { get($_[0]->nth(0),$_[0]->nth(1)) },
    'contains?' => sub { contains_Q($_[0]->nth(0),$_[0]->nth(1)) },
    'keys' => sub { mal_keys(@{$_[0]->{val}}) },
    'vals' => sub { mal_vals(@{$_[0]->{val}}) },

    'sequential?' => sub { _sequential_Q($_[0]->nth(0)) ? $true : $false },
    'nth' => sub { nth($_[0]->nth(0), ${$_[0]->nth(1)}) },
    'first' => sub { first($_[0]->nth(0)) },
    'rest' => sub { rest($_[0]->nth(0)) },
    'cons' => sub { cons($_[0]->nth(0), $_[0]->nth(1)) },
    'concat' => sub { concat(@{$_[0]->{val}}) },
    'empty?' => sub { scalar(@{$_[0]->nth(0)->{val}}) == 0 ? $true : $false },
    'count' => sub { count($_[0]->nth(0)) },
    'apply' => sub { apply($_[0]) },
    'map' => sub { mal_map($_[0]->nth(0), $_[0]->nth(1)) },
    'conj' => sub { die "not implemented\n"; },

    'with-meta' => sub { with_meta($_[0]->nth(0), $_[0]->nth(1)) },
    'meta' => sub { meta($_[0]->nth(0)) },
    'atom' => sub { Atom->new($_[0]->nth(0)) },
    'atom?' => sub { _atom_Q($_[0]->nth(0)) ? $true : $false },
    'deref' => sub { $_[0]->nth(0)->{val} },
    'reset!' => sub { $_[0]->nth(0)->{val} = $_[0]->nth(1) },
    'swap!' => sub { swap_BANG(@{$_[0]->{val}}) },
};

1;
