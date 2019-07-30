package types;
use strict;
use warnings;

use Data::Dumper;
use Exporter 'import';
our @EXPORT_OK = qw(_equal_Q
                    $nil $true $false);

# General functions

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


{
    package Mal::Symbol;
    use parent -norequire, 'Mal::Scalar';
}


{
    package Mal::String;
    use parent -norequire, 'Mal::Scalar';
    # "isa" can distinguish keywords from other strings.
    sub isa {
	my $self = shift;
	return 1 if ($_[0] eq 'Mal::Keyword' && $$self =~ /^\x{029e}/);
	return $self->SUPER::isa(@_);
    }
    # Pseudo-constructor for making keywords.
    sub Mal::Keyword::new { shift; Mal::String->new("\x{029e}" . $_[0]) }
}


# Sequences

{
    package Mal::Sequence;
    use parent -norequire, 'Mal::Type';
    sub new  { my $class = shift; bless $_[0], $class }
    sub rest { my $arr = $_[0]; Mal::List->new([@$arr[1..$#$arr]]); }
    sub slice { my $arr = $_[0]; Mal::List->new([@$arr[$_[1]..$_[2]]]); }
    sub clone { my $self = shift; ref($self)->new([@$self]) }
}

# Lists

{
    package Mal::List;
    use parent -norequire, 'Mal::Sequence';
}


# Vectors

{
    package Mal::Vector;
    use parent -norequire, 'Mal::Sequence';
}


# Hash-maps

{
    package Mal::HashMap;
    use parent -norequire, 'Mal::Type';
    use List::Util qw(pairmap);
    use Scalar::Util qw(reftype);
    sub new  {
        my ($class, $src) = @_;
        if (reftype($src) eq 'ARRAY') {
            $src = {pairmap { $$a => $b } @$src};
	}
        return bless $src, $class;
    }
    sub clone { my $self = shift; ref($self)->new({%$self}) }
}


# Functions

{
    package Mal::Callable;
    use parent -norequire, 'Mal::Type';
    sub new  { my $class = shift; bless $_[0], $class }
    sub clone { my $self = shift; bless sub { goto &$self }, ref($self) }
}

{
    package Mal::Function;
    use parent -norequire, 'Mal::Callable';
}

{
    package Mal::Macro;
    use parent -norequire, 'Mal::Callable';
}


# Atoms

{
    package Mal::Atom;
    use parent -norequire, 'Mal::Type';
    sub new  { my ($class, $val) = @_; bless \$val, $class }
    sub clone { my $self = shift; ref($self)->new($$self) }
}

1;
