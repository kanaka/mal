use v5.18;

package Types;



package List;

sub new {
    my ($class, $list) = @_;
    bless $list, $class;
}

package Atom;

sub expand {
    ${$_[0]};
}



package list;
use base 'List';



package number;
use base 'Atom';

use overload
    '+' => \&plus,
    '-' => \&minus,
    '*' => \&mult,
    '/' => \&divide,
    '""' => \&expand,
    ;

sub new {
    my ($class, $atom) = @_;
    bless \$atom, $class;
}

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
