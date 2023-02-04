use v5.18;

package Types;


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
package string;
use base 'Atom';

package keyword;
use base 'Atom';

package number;
use base 'Atom';

use overload
    '+' => \&plus,
    '-' => \&minus,
    '*' => \&mult,
    '/' => \&divide,
    '""' => \&expand,
    ;

sub plus {
    my ($x, $y) = @_;
    my $class = ref($x);
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    $class->new($x + $y);
}

sub minus {
    my ($x, $y) = @_;
    my $class = ref($x);
    $x = ref($x) ? $$x : $x;
    $y = ref($y) ? $$y : $y;
    $class->new($x - $y);
}

sub mult {
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
