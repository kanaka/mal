use strict; use warnings;
package Types;

use Exporter 'import';

our @EXPORT = qw<
    atom
    boolean
    false
    function
    hash_map
    keyword
    list
    macro
    nil
    number
    string
    symbol
    true
    vector

    WWW
    XXX
>;

sub atom     { 'atom'    ->new(@_) }
sub boolean  { 'boolean' ->new(@_) }
sub function { 'function'->new(@_) }
sub keyword  { 'keyword' ->new(@_) }
sub hash_map { 'hash_map'->new(@_) }
sub list     { 'list'    ->new(@_) }
sub macro    { 'macro'   ->new(@_) }
sub number   { 'number'  ->new(@_) }
sub string   { 'string'  ->new(@_) }
sub symbol   { 'symbol'  ->new(@_) }
sub vector   { 'vector'  ->new(@_) }

sub WWW { require XXX; goto &XXX::WWW }
sub XXX { require XXX; goto &XXX::XXX }


#------------------------------------------------------------------------------
# Base Classes:
#------------------------------------------------------------------------------
package List;

sub new {
    my ($class, $list) = @_;
    bless $list, $class;
}

sub clone { ref($_[0])->new([@{$_[0]}]) }

#------------------------------------------------------------------------------
package Map;

sub new { die }

#------------------------------------------------------------------------------
package Scalar;

use overload '""' => sub { ${$_[0]} };
use overload cmp => sub { "$_[0]" cmp "$_[1]" };

sub new {
    my ($class, $scalar) = @_;
    bless \$scalar, $class;
}


#------------------------------------------------------------------------------
# List types:
#------------------------------------------------------------------------------
package list;
use base 'List';

#------------------------------------------------------------------------------
package vector;
use base 'List';

#------------------------------------------------------------------------------
package hash_map;
use base 'Map';
use Tie::IxHash;

sub new {
    my ($class, $list) = @_;
    for (my $i = 0; $i < @$list; $i += 2) {
        my $key = $list->[$i];
        if (my $type = ref($key)) {
            die "Type '$type' not supported as a hash-map key"
                if not($key->isa('Scalar')) or $type eq 'symbol';
            $list->[$i] = $type eq 'string' ? qq<"$key> : qq<$key>;
        }
    }
    my %hash;
    my $tie = tie(%hash, 'Tie::IxHash', @$list);
    my $hash = \%hash;
    bless $hash, $class;
}

sub clone {
    hash_map->new([ %{$_[0]} ]);
}

#------------------------------------------------------------------------------
# Scalar types:
#------------------------------------------------------------------------------
package function;
sub new {
    my ($class, $sig, $ast, $env) = @_;
    bless sub {
        $ast,
        Env->new(
            outer => $env,
            binds => $sig,
            exprs => \@_,
        );
    }, $class;
}
sub clone {
    my ($fn) = @_;
    bless sub { goto &$fn }, ref($fn);
}

package macro;
# use base 'function';
sub new {
    my ($class, $function) = @_;
    XXX $function unless ref($function) eq 'function';
    bless sub { goto &$function }, $class;
}


package atom;
sub new {
    bless [$_[1] // die], $_[0];
}


package symbol;
use base 'Scalar';


package string;
use base 'Scalar';


package keyword;
use base 'Scalar';
sub new {
    my ($class, $scalar) = @_;
    $scalar =~ s/^://;
    $scalar = ":$scalar";
    bless \$scalar, $class;
}



package nil;
use base 'Scalar';

{
    package Types;
    my $n;
    BEGIN { $n = 1 }
    use constant nil => bless \$n, 'nil';
}


package boolean;
use base 'Scalar';

{
    package Types;
    my ($t, $f);
    BEGIN { ($t, $f) = (1, 0) }
    use constant true => bless \$t, 'boolean';
    use constant false => bless \$f, 'boolean';
}

sub new {
    my ($class, $scalar) = @_;
    my $type = ref($scalar);
    (not $type) ? $scalar ? Types::true : Types::false :
    $type eq 'nil' ? Types::false :
    $type eq 'boolean' ? $scalar :
    Types::true;
}


package number;
use base 'Scalar';

sub boolean { boolean->new(@_) }

use overload
    '""' => sub { ${$_[0]} },
    '+' => \&add,
    '-' => \&subtract,
    '*' => \&multiply,
    '/' => \&divide,
    '=' => \&equal_to,
    '>' => \&greater_than,
    '>=' => \&greater_equal,
    '<' => \&less_than,
    '<=' => \&less_equal,
    ;

sub greater_than {
    my ($x, $y) = @_;
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    boolean($x > $y);
}

sub greater_equal {
    my ($x, $y) = @_;
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    boolean($x >= $y);
}

sub less_than {
    my ($x, $y) = @_;
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    boolean($x < $y);
}

sub less_equal {
    my ($x, $y) = @_;
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    boolean($x <= $y);
}

sub add {
    my ($x, $y) = @_;
    my $class = ref($x);
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    $class->new($x + $y);
}

sub subtract {
    my ($x, $y) = @_;
    my $class = ref($x);
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    $class->new($x - $y);
}

sub multiply {
    my ($x, $y) = @_;
    my $class = ref($x);
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    $class->new($x * $y);
}

sub divide {
    my ($x, $y) = @_;
    my $class = ref($x);
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    $class->new(int($x / $y));
}

1;
