package printer;
use strict;
use warnings FATAL => qw(all);
use Exporter 'import';
our @EXPORT_OK = qw( _pr_str );

use types qw($nil $true $false);

use Data::Dumper;

sub _pr_str {
    my($obj, $print_readably) = @_;
    my($_r) = (defined $print_readably) ? $print_readably : 1;
    if ($obj->isa('List')) {
	return '(' . join(' ', map {_pr_str($_, $_r)} @{$obj}) . ')';
    } elsif ($obj->isa('Vector')) {
	return '[' . join(' ', map {_pr_str($_, $_r)} @{$obj}) . ']';
    } elsif ($obj->isa('HashMap')) {
	my @elems = ();

	foreach my $key (keys %$obj) {
	    push(@elems, _pr_str(String->new($key), $_r));
	    push(@elems, _pr_str($obj->{$key}, $_r));
	}

	return '{' . join(' ', @elems) . '}';
    } elsif ($obj->isa('String')) {
	if ($$obj =~ /^\x{029e}/) {
	    return ':' . substr($$obj,1);
	} elsif ($_r) {
	    my $str = $$obj;
	    $str =~ s/\\/\\\\/g;
	    $str =~ s/"/\\"/g;
	    $str =~ s/\n/\\n/g;
	    return '"' . $str . '"';
	} else {
	    return $$obj;
	}
    } elsif ($obj->isa('Function') || $obj->isa('FunctionRef')) {
	return '<fn* ' . _pr_str($obj->{params}) .
	    ' ' . _pr_str($obj->{ast}) . '>';
    } elsif ($obj->isa('Atom')) {
	return '(atom ' . _pr_str($$obj) . ")";
    } elsif ($obj->isa('CoreFunction')) {
        return '<builtin_fn* ' . $obj . '>';
    } else {
        return $$obj;
    }
}

1;
