package types;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw( $nil $true $false);

{
    package Nil;
    #sub new { my $class = shift; bless {type=>'nil'} => $class } 
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

{
    package String;
    sub new  { my $class = shift; bless \$_[0] => $class }
}

{
    package List;
    sub new  { my $class = shift; bless $_[0], $class }
}

{
    package Vector;
    sub new  { my $class = shift; bless $_[0], $class }
}

{
    package HashMap;
    sub new  { my $class = shift; bless $_[0], $class }
}

1;
