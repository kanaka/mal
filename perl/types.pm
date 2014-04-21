package types;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw( $nil $true $false);

{
    package Nil;
    sub new { my $class = shift; my $s = 'nil'; bless \$s => $class }
}
{
    package True;
    sub new { my $class = shift; my $s = 'true'; bless \$s => $class }
}
{
    package False;
    sub new { my $class = shift; my $s = 'false'; bless \$s => $class }
}

our $nil =   Nil->new();
our $true =  True->new();
our $false = False->new();

{
    package Integer;
    sub new  { my $class = shift; bless \$_[0] => $class }
}


{
    package Symbol;
    sub new  { my $class = shift; bless \$_[0] => $class }
}

sub _symbol_Q { ref $_[0] =~ /^Symbol/ }


{
    package String;
    sub new  { my $class = shift; bless \$_[0] => $class }
}


{
    package List;
    use Data::Dumper;
    sub new  { my $class = shift; bless $_[0], $class }
    sub rest { my @arr = @{$_[0]}; List->new([@arr[1..$#arr]]); }
}

sub _list_Q { ref $_[0] =~ /^Symbol/ }


{
    package Vector;
    sub new  { my $class = shift; bless $_[0], $class }
}


{
    package HashMap;
    sub new  { my $class = shift; bless $_[0], $class }
}

1;
