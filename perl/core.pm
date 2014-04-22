package core;
use strict;
use warnings FATAL => qw(all);
use Exporter 'import';
our @EXPORT_OK = qw($core_ns);

use types qw(_sequential_Q _equal_Q $nil $true $false _list_Q);
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

sub slurp {
    my ($fname) = ${$_[0]}; 
    open my $F, '<', $fname or die "error opening '$fname'";
    my $data = do { local $/; <$F> };
    String->new($data)
}


# List functions

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



our $core_ns = {
    '=' =>  sub { _equal_Q($_[0][0], $_[0][1]) ? $true : $false },

    'pr-str' =>  sub { pr_str($_[0]) },
    'str' =>     sub { str($_[0]) },
    'prn' =>     sub { prn($_[0]) },
    'println' => sub { println($_[0]) },
    'read-string' => sub { read_str(${$_[0][0]}) },
    'slurp' =>   sub { slurp($_[0][0]) },
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

    'nth' => sub { nth($_[0][0], ${$_[0][1]}) },
    'first' => sub { first($_[0][0]) },
    'rest' => sub { rest($_[0][0]) },
    'cons' => sub { cons($_[0][0], $_[0][1]) },
    'concat' => sub { concat(@{$_[0]}) },
    'empty?' => sub { scalar(@{$_[0][0]}) == 0 ? $true : $false },
    'count' => sub { Integer->new(scalar(@{$_[0][0]})) },
};

1;
