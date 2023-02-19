use strict; use warnings;
package Printer;

use Types;

my $escape = {
    "\n" => "\\n",
    "\t" => "\\t",
    "\"" => "\\\"",
    "\\" => "\\\\",
};

sub pr_str {
    my ($o, $raw) = (@_, 0);
    my $type = ref $o or XXX $o;

    $type eq 'atom' ?  "(atom ${\pr_str($o->[0], $raw)})" :
    $type eq 'string' ? $raw ? $$o :
        qq{"${local $_ = $$o; s/([\n\t\"\\])/$escape->{$1}/ge; \$_}"} :
    $type eq 'symbol' ? $$o :
    $type eq 'keyword' ? $$o :
    $type eq 'number' ? $$o :
    $type eq 'boolean' ? $$o ? 'true' : 'false' :
    $type eq 'nil' ? 'nil' :
    $type eq 'function' ? '<#function>' :
    $type eq 'macro' ? '<#macro>' :
    $type eq 'list' ?
        "(${\ join(' ', map pr_str($_, $raw), @$o)})" :
    $type eq 'vector' ?
        "[${\ join(' ', map pr_str($_, $raw), @$o)}]" :
    $type eq 'hash_map' ?
        "{${\ join(' ', map {
            my ($key, $val) = ($_, $o->{$_});
            if ($key =~ /^:/) {
                $key = keyword($key);
            } elsif ($key =~ s/^\"//) {
                $key = string($key);
            }
            (pr_str($key, $raw), pr_str($val, $raw))
        } keys %$o)}}" :
    $type =~ /^(?:(?:quasi|(?:splice_)?un)?quote|deref)$/ ?
        "(${$type=~s/_/-/g;\$type} ${\ pr_str($o->[0], $raw)})" :
    die $o;
}

1;
