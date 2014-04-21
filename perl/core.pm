package core;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw($core_ns);

use types qw(_sequential_Q _equal_Q $nil $true $false _list_Q);
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


our $core_ns = {
    '=' =>  sub { _equal_Q($_[0][0], $_[0][1]) ? $true : $false },

    'pr-str' =>  sub { pr_str($_[0]) },
    'str' =>     sub { str($_[0]) },
    'prn' =>     sub { prn($_[0]) },
    'println' => sub { println($_[0]) },
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
    'empty?' => sub { scalar(@{$_[0][0]}) == 0 ? $true : $false },
    'count' => sub { Integer->new(scalar(@{$_[0][0]})) },
};

1;
