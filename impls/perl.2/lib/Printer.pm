package Printer;

use Mo;

use Types;

my $escape = {
    "\n" => "\\n",
    "\t" => "\\t",
    "\"" => "\\\"",
    "\\" => "\\\\",
};

sub pr_str {
    my ($o, $raw) = (@_, 0);
    my $type = ref $o or ::XXX $o;

    $type eq 'string' ?  $raw ? $$o :
        qq{"${$$o=~s/([\n\t\"\\])/$escape->{$1}/ge;$o}"} :
    $type eq 'symbol' ? $$o :
    $type eq 'keyword' ? $$o :
    $type eq 'number' ? $$o :
    $type eq 'boolean' ? $$o ? 'true' : 'false' :
    $type eq 'nil' ? 'nil' :
    $type eq 'function' ? '<#function>' :
    $type eq 'list' ?
        "(${\ join(' ', map pr_str($_, $raw), @$o)})" :
    $type eq 'vector' ?
        "[${\ join(' ', map pr_str($_, $raw), @$o)}]" :
    $type eq 'hash_map' ?
        "{${\ join(' ', map {
            my ($key, $val) = ($_, $o->{$_});
            $key = $key =~ /^:/ ? keyword($key) : string($key);
            (pr_str($key, $raw), pr_str($val, $raw))
        } keys %$o)}}" :
    $type =~ /^(?:(?:quasi|(?:splice_)?un)?quote|deref)$/ ?
        "(${$type=~s/_/-/g;\$type} ${\ pr_str($o->[0], $raw)})" :
    die $o;
}

1;
