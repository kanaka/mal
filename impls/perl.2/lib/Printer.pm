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
    $type eq 'hash_map' ?
        "{${\ join(' ', map {
            my ($key, $val) = ($_, $o->{$_});
            $key = $key =~ /^:/
            ? keyword->new($key)
            : string->new($key);
            (pr_str($key), pr_str($val))
        } keys %$o)}}" :
    $type =~ /^(?:(?:quasi|(?:splice_)?un)?quote|deref)$/ ?
        "(${$type=~s/_/-/g;\$type} ${\ pr_str($o->[0])})" :
    $type eq 'string' ?
        do {
            $$o =~ s/([\n\t\"\\])/$escape->{$1}/ge;
            qq{"$$o"};
        } :
    $$o;
}

1;
