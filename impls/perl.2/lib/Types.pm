use v5.10;

package Types;

use Exporter 'import';

our @EXPORT = qw<
    boolean
    false
    function
    keyword
    list
    nil
    number
    string
    symbol
    true
>;

sub boolean  { 'boolean' ->new(@_) }
sub function { 'function'->new(@_) }
sub keyword  { 'keyword' ->new(@_) }
sub list     { 'list'    ->new(@_) }
sub number   { 'number'  ->new(@_) }
sub string   { 'string'  ->new(@_) }
sub symbol   { 'symbol'  ->new(@_) }


#------------------------------------------------------------------------------
# Base Classes:
#------------------------------------------------------------------------------
package List;

sub new {
    my ($class, $list) = @_;
    bless $list, $class;
}

package Map;

sub new { die }


#------------------------------------------------------------------------------
package Atom;

use overload '""' => sub { ${$_[0]} };

sub new {
    my ($class, $atom) = @_;
    bless \$atom, $class;
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
    my %hash;
    my $tie = tie(%hash, 'Tie::IxHash', @$list);
    my $hash = \%hash;
    bless $hash, $class;
}

#------------------------------------------------------------------------------
# Atom types:
#------------------------------------------------------------------------------
package function;
sub new {
    my ($class, $sig, $ast, $env) = @_;
    my $self = bless {
        ast => $ast,
        sig => $sig,
        env => $env,
        fun => sub { Eval::eval($ast, $env) },
    }, $class;
}

package symbol;
use base 'Atom';


package string;
use base 'Atom';


package keyword;
use base 'Atom';


package nil;
use base 'Atom';

{
    package Types;
    my $n;
    BEGIN { $n = 1 }
    use constant nil => bless \$n, 'nil';
}


package boolean;
use base 'Atom';

{
    package Types;
    my ($t, $f);
    BEGIN { ($t, $f) = (1, 0) }
    use constant true => bless \$t, 'boolean';
    use constant false => bless \$f, 'boolean';
}

sub new {
    my ($class, $atom) = @_;
    my $type = ref($atom);
    (not $type) ? $atom ? Types::true : Types::false :
    $type eq 'nil' ? Types::false :
    $type eq 'boolean' ? $atom :
    Types::true;
}


package number;
use base 'Atom';

sub boolean { boolean->new(@_) }

use overload
    '+' => \&add,
    '-' => \&subtract,
    '*' => \&multiply,
    '/' => \&divide,
    '=' => \&equal_to,
    '>' => \&greater_than,
    '>=' => \&greater_equal,
    '<' => \&less_than,
    '<=' => \&less_equal,
    '""' => \&expand,
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

sub expand { ${$_[0]} }

1;
