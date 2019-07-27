package types;
use strict;
use warnings;

use Data::Dumper;
use Exporter 'import';
our @EXPORT_OK = qw(_sequential_Q _equal_Q
                    $nil $true $false
                    _number_Q _symbol _symbol_Q _string_Q _keyword _keyword_Q _list_Q _vector_Q _sub_Q _function_Q
                    _hash_map _hash_map_Q _atom_Q);
use List::Util qw(pairs pairmap);

# General functions

sub _sequential_Q {
    return _list_Q($_[0]) || _vector_Q($_[0])
}

sub _equal_Q {
    my ($a, $b) = @_;
    unless ((ref $a eq ref $b) ||
	    ($a->isa('Mal::Sequence') && $b->isa('Mal::Sequence'))) {
        return 0;
    }
    if ($a->isa('Mal::Sequence')) {
	unless (scalar(@$a) == scalar(@$b)) {
	    return 0;
	}
	for (my $i=0; $i<scalar(@$a); $i++) {
	    unless (_equal_Q($a->[$i], $b->[$i])) {
		return 0;
	    }
	}
	return 1;
    } elsif ($a->isa('Mal::HashMap')) {
	unless (scalar(keys %$a) == scalar(keys %$b)) {
	    return 0;
	}
	foreach my $k (keys %$a) {
	    unless (_equal_Q($a->{$k}, $b->{$k})) {
		return 0;
	    }
	}
	return 1;
    } else {
	return $$a eq $$b;
    }
    return 0;
}


# Errors/Exceptions

{
    package Mal::BlankException;
    sub new { my $class = shift; bless Mal::String->new("Blank Line") => $class }
}

# Superclass for all kinds of mal value

{
    package Mal::Type;
}

# Scalars

{
    package Mal::Scalar;
    use parent -norequire, 'Mal::Type';
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
    use parent -norequire, 'Mal::Type';
    use overload '@{}' => sub { $_[0]->{val} }, fallback => 1;
    sub new  { my $class = shift; bless {'val'=>$_[0]}, $class }
    #sub _val { $_[0]->{val}->[$_[1]]->{val}; } # return value of nth item
    sub rest { my @arr = @{$_[0]->{val}}; Mal::List->new([@arr[1..$#arr]]); }
    sub slice { my @arr = @{$_[0]->{val}}; Mal::List->new([@arr[$_[1]..$_[2]]]); }
    sub clone { my $self = shift; ref($self)->new([@$self]) }
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
    use parent -norequire, 'Mal::Type';
    sub new  { my $class = shift; bless $_[0], $class }
    sub clone { my $self = shift; ref($self)->new({%$self}) }
}

sub _hash_map { Mal::HashMap->new( { pairmap { $$a => $b } @_ } ) }

sub _hash_map_Q { $_[0]->isa('Mal::HashMap') }


# Functions

{
    package Mal::Function;
    use parent -norequire, 'Mal::Type';
    use overload '&{}' => sub { my $f = shift; sub { $f->apply(\@_) } },
                 fallback => 1;
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
        return Mal::Env->new($self->{env}, $self->{params}, $_[1]);
    }
    sub apply {
        my $self = $_[0];
        return &{ $self->{eval} }($self->{ast}, gen_env($self, $_[1]));
    }
    sub clone {	my $self = shift; bless { %$self }, ref($self) }
}

sub _sub_Q { $_[0]->isa('Mal::CoreFunction') ||  $_[0]->isa('Mal::FunctionRef') }
sub _function_Q { $_[0]->isa('Mal::Function') }


# FunctionRef

{
    package Mal::FunctionRef;
    use parent -norequire, 'Mal::Type';
    use overload '&{}' => sub { $_[0]->{code} }, fallback => 1;
    sub new {
        my ($class, $code) = @_;
        bless {'code'=>$code}, $class
    }
    sub clone { my $self = shift; ref($self)->new($self->{code}) }
}

# Core Functions

{
    package Mal::CoreFunction;
    sub clone { my $self = shift; FunctionRef->new($self) }
}


# Atoms

{
    package Mal::Atom;
    use parent -norequire, 'Mal::Type';
    use overload '${}' => sub { \($_[0]->{val}) }, fallback => 1;
    sub new  { my $class = shift; bless {'val'=>$_[0]}, $class }
    sub clone { my $self = shift; ref($self)->new($$self) }
}

sub _atom_Q { $_[0]->isa('Mal::Atom') }

1;
