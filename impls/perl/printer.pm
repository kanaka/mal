package printer;
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = qw( _pr_str );

use types qw(thaw_key $nil $true $false);

use Data::Dumper;
use List::Util qw(pairmap);

sub _pr_str {
    my($obj, $print_readably) = @_;
    my($_r) = $print_readably // 1;
    if ($obj->isa('Mal::List')) {
	return '(' . join(' ', map { _pr_str($_, $_r) } @$obj) . ')';
    } elsif ($obj->isa('Mal::Vector')) {
	return '[' . join(' ', map { _pr_str($_, $_r) } @$obj) . ']';
    } elsif ($obj->isa('Mal::HashMap')) {
	return '{' . join(' ', pairmap { _pr_str(thaw_key($a), $_r) =>
				         _pr_str($b, $_r) } %$obj) . '}';
    } elsif ($obj->isa('Mal::Keyword')) {
	return ":$$obj";
    } elsif ($obj->isa('Mal::String')) {
	if ($_r) {
	    my $str = $$obj;
	    $str =~ s/\\/\\\\/g;
	    $str =~ s/"/\\"/g;
	    $str =~ s/\n/\\n/g;
	    return qq'"$str"';
	} else {
	    return $$obj;
	}
    } elsif ($obj->isa('Mal::Atom')) {
	return '(atom ' . _pr_str($$obj) . ")";
    } elsif ($obj->isa('Mal::Function')) {
        return "<fn* $obj>";
    } elsif ($obj->isa('Mal::Macro')) {
        return "<macro* $obj>";
    } else {
        return $$obj;
    }
}

1;
