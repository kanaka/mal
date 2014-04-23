package core;
use strict;
use warnings FATAL => qw(all);
use Exporter 'import';
our @EXPORT_OK = qw($core_ns);

use readline qw(readline);
use types qw(_sequential_Q _equal_Q $nil $true $false
             _symbol_Q _nil_Q _true_Q _false_Q _list_Q
             _hash_map _hash_map_Q _assoc_BANG _dissoc_BANG);
use reader qw(read_str);
use printer qw(_pr_str);

use Data::Dumper;

# String functions

sub pr_str {
    return String->new(join(" ", map {_pr_str($_, 1)} @{$_[0]}));
}

sub str {
    return String->new(join("", map {_pr_str($_, 0)} @{$_[0]}));
}

sub prn {
    print join(" ", map {_pr_str($_, 1)} @{$_[0]}) . "\n";
    return $nil
}

sub println {
    print join(" ", map {_pr_str($_, 0)} @{$_[0]}) . "\n";
    return $nil
}

sub mal_readline {
    my $line = readline(${$_[0]});
    return $line ? String->new($line) : $nil;
}

sub slurp {
    my ($fname) = ${$_[0]}; 
    open my $F, '<', $fname or die "error opening '$fname'";
    my $data = do { local $/; <$F> };
    String->new($data)
}

# Hash Map functions

sub assoc {
    my $src_hsh = shift;
    my $new_hsh = { %$src_hsh };
    return _assoc_BANG($new_hsh, @_);
}

sub dissoc {
    my $src_hsh = shift;
    my $new_hsh = { %$src_hsh };
    return _dissoc_BANG($new_hsh, @_);
}


sub get {
    my ($hsh, $key) = @_;
    return $nil if $hsh eq $nil;
    return exists $hsh->{$$key} ? $hsh->{$$key} : $nil;
}

sub contains_Q {
    my ($hsh, $key) = @_;
    return $nil if $hsh eq $false;
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
    my @new_arr = @{[$a]};
    push @new_arr, @$b;
    List->new(\@new_arr);
}

sub concat {
    if (scalar(@_) == 0) { return List->new([]); }
    my ($a) = shift;
    my @new_arr = @{$a};
    map { push @new_arr, @$_ } @_;
    List->new(\@new_arr);
}

sub nth { my ($seq,$i) = @_; return scalar(@$seq) > $i ? $seq->[$i] : $nil; }

sub first { my ($seq) = @_; return scalar(@$seq) > 0 ? $seq->[0] : $nil; }

sub rest { return $_[0]->rest(); }

sub apply {
    my @all_args = @{$_[0]};
    my $f = $all_args[0];
    my @apply_args = @all_args[1..$#all_args];
    my @args = @apply_args[0..$#apply_args-1];
    push @args, @{$apply_args[$#apply_args]};
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
        @arr = map { $f->apply(List->new([$_])) } @{$_[0]};
    } else {
        @arr = map { &{ $f}(List->new([$_])) } @{$_[0]};
    }
    return List->new(\@arr);
}



our $core_ns = {
    '=' =>  sub { _equal_Q($_[0][0], $_[0][1]) ? $true : $false },
    'throw' => sub { die $_[0][0] },
    'nil?' => sub { _nil_Q($_[0][0]) ? $true : $false },
    'true?' => sub { _true_Q($_[0][0]) ? $true : $false },
    'false?' => sub { _false_Q($_[0][0]) ? $true : $false },
    'symbol?' => sub { _symbol_Q($_[0][0]) ? $true : $false },

    'pr-str' =>  sub { pr_str($_[0]) },
    'str' =>     sub { str($_[0]) },
    'prn' =>     sub { prn($_[0]) },
    'println' => sub { println($_[0]) },
    'readline' =>    sub { mal_readline($_[0][0]) },
    'read-string' => sub { read_str(${$_[0][0]}) },
    'slurp' =>       sub { slurp($_[0][0]) },
    '<' =>  sub { ${$_[0][0]} < ${$_[0][1]} ? $true : $false },
    '<=' => sub { ${$_[0][0]} <= ${$_[0][1]} ? $true : $false },
    '>' =>  sub { ${$_[0][0]} > ${$_[0][1]} ? $true : $false },
    '>=' => sub { ${$_[0][0]} >= ${$_[0][1]} ? $true : $false },
    '+' =>  sub { Integer->new(${$_[0][0]} + ${$_[0][1]})},
    '-' =>  sub { Integer->new(${$_[0][0]} - ${$_[0][1]})},
    '*' =>  sub { Integer->new(${$_[0][0]} * ${$_[0][1]})},
    '/' =>  sub { Integer->new(${$_[0][0]} / ${$_[0][1]})},

    'list'  => sub { $_[0] },
    'list?' => sub { _list_Q($_[0][0]) ? $true : $false },
    'hash-map' => sub { _hash_map(@{$_[0]}) },
    'map?' => sub { _hash_map_Q($_[0][0]) ? $true : $false },
    'assoc' => sub { assoc(@{$_[0]}) },
    'dissoc' => sub { dissoc(@{$_[0]}) },
    'get' => sub { get($_[0][0],$_[0][1]) },
    'contains?' => sub { contains_Q($_[0][0],$_[0][1]) },
    'keys' => sub { mal_keys(@{$_[0]}) },
    'vals' => sub { mal_vals(@{$_[0]}) },

    'sequential?' => sub { _sequential_Q($_[0][0]) ? $true : $false },
    'nth' => sub { nth($_[0][0], ${$_[0][1]}) },
    'first' => sub { first($_[0][0]) },
    'rest' => sub { rest($_[0][0]) },
    'cons' => sub { cons($_[0][0], $_[0][1]) },
    'concat' => sub { concat(@{$_[0]}) },
    'empty?' => sub { scalar(@{$_[0][0]}) == 0 ? $true : $false },
    'count' => sub { Integer->new(scalar(@{$_[0][0]})) },
    'apply' => sub { apply($_[0]) },
    'map' => sub { mal_map($_[0][0], $_[0][1]) },
};

1;
