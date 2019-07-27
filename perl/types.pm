package types;
use strict;
use warnings FATAL => qw(all);
use Exporter 'import';
our @EXPORT_OK = qw(_sequential_Q _equal_Q _clone
                    $nil $true $false _nil_Q _true_Q _false_Q
                    _number_Q _symbol _symbol_Q _string_Q _keyword _keyword_Q _list_Q _vector_Q _sub_Q _function_Q
                    _hash_map _hash_map_Q _atom_Q);
use List::Util qw(pairs pairmap);

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
    if ($a->isa('Mal::Symbol')) {
	return $$a eq $$b;
    } elsif ($a->isa('Mal::Sequence')) {
	if (! (scalar(@$a) == scalar(@$b))) {
	    return 0;
	}
	for (my $i=0; $i<scalar(@$a); $i++) {
	    if (! _equal_Q($a->[$i], $b->[$i])) {
		return 0;
	    }
	}
	return 1;
    } elsif ($a->isa('Mal::HashMap')) {
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
    if ($obj->isa('Mal::CoreFunction')) {
	return Mal::FunctionRef->new( $obj );
    } else {
	return bless {%{$obj}}, ref $obj;
    }
}

# Errors/Exceptions

{
    package Mal::BlankException;
    sub new { my $class = shift; bless Mal::String->new("Blank Line") => $class }
}

# Scalars

{
    package Mal::Scalar;
    sub new { my ($class, $value) = @_; bless \$value, $class }
}

{
    package Mal::Nil;
    use parent -norequire, 'Mal::Scalar';
    # Allow nil to be treated as an empty list or hash-map.
    use overload '@{}' => sub { [] }, '%{}' => sub { {} }, fallback => 1;
    sub rest { Mal::List->new([]) }
}
{
    package Mal::True;
    use parent -norequire, 'Mal::Scalar';
}
{
    package Mal::False;
    use parent -norequire, 'Mal::Scalar';
}

our $nil =   Mal::Nil->new('nil');
our $true =  Mal::True->new('true');
our $false = Mal::False->new('false');

sub _nil_Q   { return $_[0] eq $nil }
sub _true_Q  { return $_[0] eq $true }
sub _false_Q { return $_[0] eq $false }


{
    package Mal::Integer;
    use parent -norequire, 'Mal::Scalar';
}
sub _number_Q { $_[0]->isa('Mal::Integer') }


{
    package Mal::Symbol;
    use parent -norequire, 'Mal::Scalar';
}
sub _symbol_Q { $_[0]->isa('Mal::Symbol') }


sub _string_Q { $_[0]->isa('Mal::String') && ${$_[0]} !~ /^\x{029e}/; }


sub _keyword { return Mal::String->new(("\x{029e}".$_[0])); }
sub _keyword_Q { $_[0]->isa('Mal::String') && ${$_[0]} =~ /^\x{029e}/; }


{
    package Mal::String;
    use parent -norequire, 'Mal::Scalar';
}


# Sequences

{
    package Mal::Sequence;
    use overload '@{}' => sub { $_[0]->{val} }, fallback => 1;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub meta { $_[0]->{meta} }
    #sub _val { $_[0]->{val}->[$_[1]]->{val}; } # return value of nth item
    sub rest { my @arr = @{$_[0]->{val}}; Mal::List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]->{val}}; Mal::List->new([@arr[$_[1]..$_[2]]]); }
}

# Lists

{
    package Mal::List;
    use parent -norequire, 'Mal::Sequence';
}

sub _list_Q { $_[0]->isa('Mal::List') }


# Vectors

{
    package Mal::Vector;
    use parent -norequire, 'Mal::Sequence';
}

sub _vector_Q { $_[0]->isa('Mal::Vector') }


# Hash Maps

{
    package Mal::HashMap;
    use overload '%{}' => sub { no overloading '%{}'; $_[0]->{val} },
	         fallback => 1;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub meta { no overloading '%{}'; $_[0]->{meta} }
}

sub _hash_map { Mal::HashMap->new( { pairmap { $$a => $b } @_ } ) }

sub _hash_map_Q { $_[0]->isa('Mal::HashMap') }


# Functions

{
    package Mal::Function;
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
        return Mal::Env->new($self->{env}, $self->{params}, $_[1]);
    }
    sub apply {
        my $self = $_[0];
        return &{ $self->{eval} }($self->{ast}, gen_env($self, $_[1]));
    }
}

sub _sub_Q { $_[0]->isa('Mal::CoreFunction') ||  $_[0]->isa('Mal::FunctionRef') }
sub _function_Q { $_[0]->isa('Mal::Function') }


# FunctionRef

{
    package Mal::FunctionRef;
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
    package Mal::CoreFunction;
    sub meta { $nil }
}


# Atoms

{
    package Mal::Atom;
    use overload '${}' => sub { \($_[0]->{val}) }, fallback => 1;
    sub new  { my $class = shift; bless {'meta'=>$nil, 'val'=>$_[0]}, $class }
    sub meta { $_[0]->{meta} }
}

sub _atom_Q { $_[0]->isa('Mal::Atom') }

1;
