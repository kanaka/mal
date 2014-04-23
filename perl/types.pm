package types;
use strict;
use warnings FATAL => qw(all);
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw(_sequential_Q _equal_Q
                    $nil $true $false
                    _symbol_Q _nil_Q _true_Q _false_Q _list_Q
                    _hash_map _hash_map_Q _assoc_BANG _dissoc_BANG);

use Data::Dumper;

# General functions

sub _sequential_Q {
    return _list_Q($_[0]) || _vector_Q($_[0])
}

sub _equal_Q {
    my ($a, $b) = @_;
    my ($ota, $otb) = (ref $a, ref $b);
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

sub _nil_Q   { return $_[0] eq $nil }
sub _true_Q  { return $_[0] eq $true }
sub _false_Q { return $_[0] eq $false }


{
    package Integer;
    sub new  { my $class = shift; bless \$_[0] => $class }
}


{
    package Symbol;
    sub new  { my $class = shift; bless \$_[0] => $class }
}

sub _symbol_Q { (ref $_[0]) =~ /^Symbol/ }


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
    sub rest { my @arr = @{$_[0]}; List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]}; List->new([@arr[$_[1]..$_[2]]]); }
}

sub _vector_Q { (ref $_[0]) =~ /^Vector/ }


# Hash Maps

{
    package HashMap;
    sub new  { my $class = shift; bless $_[0], $class }
}

sub _hash_map {
    my $hsh = {};
    return _assoc_BANG($hsh, @_);
}

sub _assoc_BANG {
    my $hsh = shift;
    my @lst = @_;
    for(my $i=0; $i<scalar(@lst); $i+=2) {
        my $str = $lst[$i];
        $hsh->{$$str} = $lst[$i+1];
    }
    return HashMap->new($hsh);
}

sub _dissoc_BANG {
    my $hsh = shift;
    my @lst = @_;
    for(my $i=0; $i<scalar(@lst); $i++) {
        my $str = $lst[$i];
        delete $hsh->{$$str};
    }
    return HashMap->new($hsh);
}

sub _hash_map_Q { (ref $_[0]) =~ /^HashMap/ }


# Functions

{
    package Function;
    sub new  {
        my $class = shift;
        my ($eval, $ast, $env, $params) = @_;
        bless {'eval'=>$eval,
               'ast'=>$ast,
               'env'=>$env,
               'params'=>$params,
               'ismacro'=>0}, $class
    }
    sub gen_env {
        my $self = $_[0];
        return Env->new($self->{env}, $self->{params}, $_[1]);
    }
    sub apply {
        my $self = $_[0];
        return &{ $self->{eval} }($self->{ast}, gen_env($self, $_[1]));
    }
}

1;
