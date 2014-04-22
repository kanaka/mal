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
    my ($a, $b) = @_;
    my @new_arr = @{$a};
    push @new_arr, @$b;
    List->new(\@new_arr);
}


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

    'cons' => sub { cons($_[0][0], $_[0][1]) },
    'concat' => sub { concat($_[0][0], $_[0][1]) },
    'empty?' => sub { scalar(@{$_[0][0]}) == 0 ? $true : $false },
    'count' => sub { Integer->new(scalar(@{$_[0][0]})) },
};

1;
