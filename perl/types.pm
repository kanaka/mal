package types;
use strict;
use warnings FATAL => qw(all);
use Exporter 'import';
our @EXPORT_OK = qw(_sequential_Q _equal_Q _clone
                    $nil $true $false _nil_Q _true_Q _false_Q
                    _number_Q _symbol _symbol_Q _string_Q _keyword _keyword_Q _list_Q _vector_Q _sub_Q _function_Q
                    _hash_map _hash_map_Q _assoc_BANG _atom_Q);
use List::Util qw(pairs);

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
    if ($a->isa('Symbol')) {
	return $$a eq $$b;
    } elsif ($a->isa('Sequence')) {
	if (! (scalar(@$a) == scalar(@$b))) {
	    return 0;
	}
	for (my $i=0; $i<scalar(@$a); $i++) {
	    if (! _equal_Q($a->[$i], $b->[$i])) {
		return 0;
	    }
	}
	return 1;
    } elsif ($a->isa('HashMap')) {
	if (! (scalar(keys %$a) == scalar(keys %$b))) {
	    return 0;
	}
	foreach my $k (keys %$a) {
	    if (!_equal_Q($a->{$k}, $b->{$k})) {
		return 0;
	    }
	}
	return 1;
    } else {
	return $$a eq $$b;
    }
    return 0;
}

sub _clone {
    no overloading '%{}';
    my ($obj) = @_;
    if ($obj->isa('CoreFunction')) {
	return FunctionRef->new( $obj );
    } else {
	return bless {%{$obj}}, ref $obj;
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
    # Allow nil to be treated as an empty list or hash-map.
    use overload '@{}' => sub { [] }, '%{}' => sub { {} }, fallback => 1;
    sub new { my $class = shift; my $s = 'nil'; bless \$s => $class }
    sub rest { List->new([]) }
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
sub _number_Q { $_[0]->isa('Integer') }


{
    package Symbol;
    sub new  { my $class = shift; bless \do { my $x=$_[0] }, $class }
}
sub _symbol_Q { $_[0]->isa('Symbol') }


sub _string_Q { $_[0]->isa('String') && ${$_[0]} !~ /^\x{029e}/; }


sub _keyword { return String->new(("\x{029e}".$_[0])); }
sub _keyword_Q { $_[0]->isa('String') && ${$_[0]} =~ /^\x{029e}/; }


{
    package String;
    sub new  { my $class = shift; bless \$_[0] => $class }
}


# Sequences

{
    package Sequence;
    use overload '@{}' => sub { $_[0]->{val} }, fallback => 1;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub meta { $_[0]->{meta} }
    #sub _val { $_[0]->{val}->[$_[1]]->{val}; } # return value of nth item
    sub rest { my @arr = @{$_[0]->{val}}; List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]->{val}}; List->new([@arr[$_[1]..$_[2]]]); }
}

# Lists

{
    package List;
    use parent -norequire, 'Sequence';
}

sub _list_Q { $_[0]->isa('List') }


# Vectors

{
    package Vector;
    use parent -norequire, 'Sequence';
}

sub _vector_Q { $_[0]->isa('Vector') }


# Hash Maps

{
    package HashMap;
    use overload '%{}' => sub { no overloading '%{}'; $_[0]->{val} },
	         fallback => 1;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub meta { no overloading '%{}'; $_[0]->{meta} }
    sub get { no overloading '%{}'; $_[0]->{val}->{$_[1]}; }
}

sub _hash_map {
    my $hsh = {};
    return _assoc_BANG($hsh, @_);
}

sub _assoc_BANG {
    my $hsh = shift;
    foreach my $pair (pairs @_) {
	my ($k, $v) = @$pair;
        $hsh->{$$k} = $v;
    }
    return HashMap->new($hsh);
}

sub _hash_map_Q { $_[0]->isa('HashMap') }


# Functions

{
    package Function;
    use overload '&{}' => sub { my $f = shift; sub { $f->apply(\@_) } },
                 fallback => 1;
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
    sub meta { $_[0]->{meta} }
    sub gen_env {
        my $self = $_[0];
        return Env->new($self->{env}, $self->{params}, $_[1]);
    }
    sub apply {
        my $self = $_[0];
        return &{ $self->{eval} }($self->{ast}, gen_env($self, $_[1]));
    }
}

sub _sub_Q { $_[0]->isa('CoreFunction') ||  $_[0]->isa('FunctionRef') }
sub _function_Q { $_[0]->isa('Function') }


# FunctionRef

{
    package FunctionRef;
    use overload '&{}' => sub { $_[0]->{code} }, fallback => 1;
    sub new {
        my ($class, $code) = @_;
        bless {'meta'=>$nil,
               'code'=>$code}, $class
    }
    sub meta { $_[0]->{meta} }
}

# Core Functions

{
    package CoreFunction;
    sub meta { $nil }
}


# Atoms

{
    package Atom;
    use overload '${}' => sub { \($_[0]->{val}) }, fallback => 1;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub meta { $_[0]->{meta} }
}

sub _atom_Q { $_[0]->isa('Atom') }

1;
