package types;
use strict;
use warnings FATAL => qw(all);
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw(_sequential_Q _equal_Q
                    $nil $true $false
                    _list_Q);

use Data::Dumper;

# General functions

sub _sequential_Q {
    return _list_Q($_[0]) || _vector_Q($_[0])
}

sub _equal_Q {
    my ($a, $b) = @_;
    my ($ota, $otb) = (ref $a, ref $b);
    #my $ota = ref $a;
    #my $otb = ref $b;
    if (!(($ota eq $otb) || (_sequential_Q($a) && _sequential_Q($b)))) {
        return 0;
    }
    given (ref $a) {
        when (/^Symbol/) {
            return $$a eq $$b;
        }
        when (/^List/ || /^Vector/) {
            if (! scalar(@$a) == scalar(@$b)) {
                return 0;
            }
            for (my $i=0; $i<scalar(@$a); $i++) {
                if (! _equal_Q($a->[$i], $b->[$i])) {
                    return 0;
                }
            }
            return 1;
        }
        default {
            return $$a eq $$b;
        }
    }
    return 0;
}

# Scalars

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


# Lists

{
    package List;
    sub new  { my $class = shift; bless $_[0], $class }
    sub rest { my @arr = @{$_[0]}; List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]}; List->new([@arr[$_[1]..$_[2]]]); }
}

sub _list_Q { (ref $_[0]) =~ /^List/ }


# Vectors

{
    package Vector;
    sub new  { my $class = shift; bless $_[0], $class }
}

sub _vector_Q { (ref $_[0]) =~ /^Vector/ }


# Hash Maps

{
    package HashMap;
    sub new  { my $class = shift; bless $_[0], $class }
}


# Functions

{
    package Function;
    sub new  {
        my $class = shift;
        my ($eval, $ast, $env, $params) = @_;
        bless {'eval'=>$eval,
               'ast'=>$ast,
               'env'=>$env,
               'params'=>$params}, $class
    }
    sub gen_env {
        my %self = %{$_[0]};
        return Env->new($self{env}, $self{params}, $_[1]);
    }
    sub apply {
        my %self = %{$_[0]};
        return &{ $self{eval} }($self{ast}, gen_env($_[1]));
    }
}

1;
