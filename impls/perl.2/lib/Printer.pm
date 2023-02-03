package Printer;

use Mo;

my $escape = {
    "\n" => "\\n",
    "\t" => "\\t",
    "\"" => "\\\"",
    "\\" => "\\\\",
};

sub pr_str {
    my ($o) = @_;
    my $type = ref $o;

    $type eq 'list' ?
        "(${\ join(' ', map pr_str($_), @$o)})" :
    $type eq 'vector' ?
        "[${\ join(' ', map pr_str($_), @$o)}]" :
    $type eq 'hash-map' ?
        "{${\ join(' ', map {
            (pr_str($_), pr_str($o->[0]{$$_}))
        } @{$o->[1]})}}" :
    $type =~ /^(?:(?:quasi|(?:splice-)?un)?quote|deref)$/ ?
        "($type ${\ pr_str($o->[0])})" :
    $type eq 'string' ?
        do {
            $$o =~ s/([\n\t\"\\])/$escape->{$1}/ge;
            qq{"$$o"};
        } :
    $$o;
}

1;
