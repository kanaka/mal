package printer;
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = qw( _pr_str );

use types qw($nil $true $false);

use Data::Dumper;

sub _pr_str {
    my($obj, $print_readably) = @_;
    my($_r) = (defined $print_readably) ? $print_readably : 1;
    if ($obj->isa('Mal::List')) {
	return '(' . join(' ', map {_pr_str($_, $_r)} @{$obj}) . ')';
    } elsif ($obj->isa('Mal::Vector')) {
	return '[' . join(' ', map {_pr_str($_, $_r)} @{$obj}) . ']';
    } elsif ($obj->isa('Mal::HashMap')) {
	my @elems = ();

	foreach my $key (keys %$obj) {
	    push(@elems, _pr_str(Mal::String->new($key), $_r));
	    push(@elems, _pr_str($obj->{$key}, $_r));
	}

	return '{' . join(' ', @elems) . '}';
    } elsif ($obj->isa('Mal::String')) {
	if ($$obj =~ /^\x{029e}/) {
	    return ":$'";
	} elsif ($_r) {
	    my $str = $$obj;
	    $str =~ s/\\/\\\\/g;
	    $str =~ s/"/\\"/g;
	    $str =~ s/\n/\\n/g;
	    return '"' . $str . '"';
	} else {
	    return $$obj;
	}
    } elsif ($obj->isa('Mal::Atom')) {
	return '(atom ' . _pr_str($$obj) . ")";
    } elsif ($obj->isa('Mal::Function')) {
        return '<fn* ' . $obj . '>';
    } elsif ($obj->isa('Mal::Macro')) {
        return '<macro* ' . $obj . '>';
    } else {
        return $$obj;
    }
}

1;
