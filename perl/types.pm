package types;
use strict;
use warnings FATAL => qw(all);
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);
use Exporter 'import';
our @EXPORT_OK = qw(_sequential_Q _equal_Q _clone
                    $nil $true $false _nil_Q _true_Q _false_Q
                    _symbol _symbol_Q _keyword _keyword_Q _list_Q _vector_Q
                    _hash_map _hash_map_Q _assoc_BANG _dissoc_BANG _atom_Q);

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
            if (! (scalar(@{$a->{val}}) == scalar(@{$b->{val}}))) {
                return 0;
            }
            for (my $i=0; $i<scalar(@{$a->{val}}); $i++) {
                if (! _equal_Q($a->nth($i), $b->nth($i))) {
                    return 0;
                }
            }
            return 1;
        }
        when (/^HashMap/) {
            if (! (scalar(keys %{ $a->{val} }) == scalar(keys %{ $b->{val} }))) {
                return 0;
            }
            foreach my $k (keys %{ $a->{val} }) {
                if (!_equal_Q($a->{val}->{$k}, $b->{val}->{$k})) {
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

sub _clone {
    my ($obj) = @_;
    given (ref $obj) {
        when (/^CODE/) {
            return FunctionRef->new( $obj );
        }
        default {
            return bless {%{$obj}}, ref $obj;
        }
    }
}

# Errors/Exceptions

{
    package BlankException;
    sub new { my $class = shift; bless String->new("Blank Line") => $class }
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
    sub new  { my $class = shift; bless \do { my $x=$_[0] }, $class }
}


{
    package Symbol;
    sub new  { my $class = shift; bless \do { my $x=$_[0] }, $class }
}
sub _symbol_Q { (ref $_[0]) =~ /^Symbol/ }


sub _keyword { return String->new(("\x{029e}".$_[0])); }
sub _keyword_Q { ((ref $_[0]) =~ /^String/) && ${$_[0]} =~ /^\x{029e}/; }


{
    package String;
    sub new  { my $class = shift; bless \$_[0] => $class }
}


# Lists

{
    package List;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub nth { $_[0]->{val}->[$_[1]]; }
    #sub _val { $_[0]->{val}->[$_[1]]->{val}; } # return value of nth item
    sub rest { my @arr = @{$_[0]->{val}}; List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]->{val}}; List->new([@arr[$_[1]..$_[2]]]); }
}

sub _list_Q { (ref $_[0]) =~ /^List/ }


# Vectors

{
    package Vector;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub nth { $_[0]->{val}->[$_[1]]; }
    #sub _val { $_[0]->{val}->[$_[1]]->{val}; } # return value of nth item
    sub rest { my @arr = @{$_[0]->{val}}; List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]->{val}}; List->new([@arr[$_[1]..$_[2]]]); }
}

sub _vector_Q { (ref $_[0]) =~ /^Vector/ }


# Hash Maps

{
    package HashMap;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub get { $_[0]->{val}->{$_[1]}; }
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
        bless {'meta'=>$nil,
               'eval'=>$eval,
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


# FunctionRef

{
    package FunctionRef;
    sub new {
        my ($class, $code) = @_;
        bless {'meta'=>$nil,
               'code'=>$code}, $class
    }
    sub apply {
        my $self = $_[0];
        return &{ $self->{code} }($_[1]);
    }
}


# Atoms

{
    package Atom;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
}

sub _atom_Q { (ref $_[0]) =~ /^Atom/ }

1;
