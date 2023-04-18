package Types;
use re '/msx';
use strict;
use warnings;

use Exporter 'import';

our @EXPORT_OK = qw(equal_q thaw_key
  nil true false);

## no critic (Modules::ProhibitMultiplePackages)

# General functions

sub equal_q {
    my ( $a, $b ) = @_;
    if ( $a->isa('Mal::Sequence') ) {
        $b->isa('Mal::Sequence')     or return 0;
        scalar @{$a} == scalar @{$b} or return 0;
        for ( 0 .. $#{$a} ) {
            equal_q( $a->[$_], $b->[$_] ) or return 0;
        }
        return 1;
    }
    ref $b eq ref $a or return 0;
    if ( $a->isa('Mal::HashMap') ) {
        scalar keys %{$a} == scalar keys %{$b} or return 0;
        while ( my ( $k, $v ) = each %{$a} ) {
            equal_q( $v, $b->{$k} ) or return 0;
        }
        return 1;
    }
    return ${$a} eq ${$b};
}

# Superclass for all kinds of mal value

{

    package Mal::Type;
}

# Scalars

{

    package Mal::Scalar;
    use parent -norequire, 'Mal::Type';

    # Overload stringification so that its result is something
    # suitable for use as a hash-map key.  The important thing here is
    # that strings and keywords are distinct: support for other kinds
    # of scalar is a bonus.
    use overload
      '""'     => sub { my $self = shift; ref($self) . q{ } . ${$self} },
      fallback => 1;

    sub new {
        my ( $class, $value ) = @_;
        return bless \$value, $class;
    }
}

# This function converts hash-map keys back into full objects

sub thaw_key {
    my ($key) = @_;
    my ( $class, $value ) = split m/[ ]/, $key, 2;
    return $class->new($value);
}

{

    package Mal::Nil;
    use parent -norequire, 'Mal::Scalar';

    # Allow nil to be treated as an empty list or hash-map.
    use overload '@{}' => sub { [] }, '%{}' => sub { {} }, fallback => 1;

}
{

    package Mal::True;
    use parent -norequire, 'Mal::Scalar';
}
{

    package Mal::False;
    use parent -norequire, 'Mal::Scalar';
}

my $nil   = Mal::Nil->new('nil');
my $true  = Mal::True->new('true');
my $false = Mal::False->new('false');
sub nil   { return $nil; }
sub true  { return $true; }
sub false { return $false; }

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
}

{

    package Mal::Keyword;
    use parent -norequire, 'Mal::Scalar';
}

# Sequences

{

    package Mal::Sequence;
    use parent -norequire, 'Mal::Type';

    sub new {
        my ( $class, $data ) = @_;
        return bless $data, $class;
    }

    sub clone {
        my $self = shift;
        return ref($self)->new( [ @{$self} ] );
    }
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

    sub new {
        my ( $class, $src ) = @_;
        return bless $src, $class;
    }

    sub clone {
        my $self = shift;
        return ref($self)->new( { %{$self} } );
    }
}

# Functions

{

    package Mal::Callable;
    use parent -norequire, 'Mal::Type';

    sub new {
        my ( $class, $data ) = @_;
        return bless $data, $class;
    }

    sub clone {
        my $self = shift;
        return bless sub { goto &{$self} }, ref $self;
    }
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

    sub new {
        my ( $class, $val ) = @_;
        return bless \$val, $class;
    }
}

1;
