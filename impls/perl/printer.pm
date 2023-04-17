package printer;
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = qw( pr_list pr_str );

use types qw(thaw_key $nil $true $false);

use List::Util qw(pairmap);

sub pr_str {
    my ( $obj, $print_readably ) = @_;
    my ($_r) = $print_readably // 1;
    if ( $obj->isa('Mal::List') ) {
        return '(' . pr_list( q{ }, $_r, @{$obj} ) . ')';
    }
    elsif ( $obj->isa('Mal::Vector') ) {
        return '[' . pr_list( q{ }, $_r, @{$obj} ) . ']';
    }
    elsif ( $obj->isa('Mal::HashMap') ) {
        return
          '{'
          . pr_list( q{ }, $_r, pairmap { thaw_key($a) => $b } %{$obj} ) . '}';
    }
    elsif ( $obj->isa('Mal::Keyword') ) {
        return ":$$obj";
    }
    elsif ( $obj->isa('Mal::String') ) {
        if ($_r) {
            my $str = $$obj;
            $str =~ s/\\/\\\\/g;
            $str =~ s/"/\\"/g;
            $str =~ s/\n/\\n/g;
            return qq'"$str"';
        }
        else {
            return $$obj;
        }
    }
    elsif ( $obj->isa('Mal::Atom') ) {
        return '(atom ' . pr_str( ${$obj} ) . ")";
    }
    elsif ( $obj->isa('Mal::Function') ) {
        return "<fn* $obj>";
    }
    elsif ( $obj->isa('Mal::Macro') ) {
        return "<macro* $obj>";
    }
    else {
        return $$obj;
    }
}

sub pr_list {
    my ( $separator, $readably, @objs ) = @_;
    return join $separator, map { pr_str( $_, $readably ) } @objs;
}

1;
