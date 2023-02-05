use v5.18;

package Types;

use Exporter 'import';

our @EXPORT = qw< num true false nil >;

sub string { string->new(@_) }
sub num { string->new(@_) }
sub true { boolean::true() }
sub false { boolean::false() }
sub nil { nil->new }


#------------------------------------------------------------------------------
# Base Classes:
#------------------------------------------------------------------------------
package List;

sub new {
    my ($class, $list) = @_;
    bless $list, $class;
}


#------------------------------------------------------------------------------
package Atom;

sub new {
    my ($class, $atom) = @_;
    bless \$atom, $class;
}

# sub expand {
#     ${$_[0]};
# }


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
use base 'List';
use Tie::IxHash;

sub new {
    my $class = shift;
    my %hash;
    my $tie = tie(%hash, 'Tie::IxHash', @_);
    my $hash = \%hash;
    bless $hash, $class;
}

#------------------------------------------------------------------------------
# Atom types:
#------------------------------------------------------------------------------
package symbol;
use base 'Atom';

use overload '""' => sub { ${$_[0]} };

package string;
use base 'Atom';

package keyword;
use base 'Atom';

package boolean;
use base 'Atom';

my $t = 1;
my $f = 0;
my $true  = do {bless \$t, 'boolean'};
my $false = do {bless \$f, 'boolean'};
sub true() { $true }
sub false() { $false }

use overload
    '""' => sub {
        ${$_[0]} ? 'true' : 'false';
    };

sub new {
    my ($class, $atom) = @_;
    my $type = ref($atom);
    (not $type) ? $atom ? true : false :
    $type eq 'nil' ? false :
    $type eq 'boolean' ? $atom :
    (not($type) and $atom =~ /^(false|nil|)$/) ? false :
    true;
}

package nil;
use base 'Atom';

use overload '""' => sub { 'nil' };

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
