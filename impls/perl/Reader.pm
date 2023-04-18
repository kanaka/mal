package Reader;
use re '/msx';
use strict;
use warnings;

use Exporter 'import';
our @EXPORT_OK = qw( read_str );

use Types qw(nil true false);

my $separators = <<'EOF';
(?: [\s,] | ; [^\n]* \n )*
EOF

my $normal = <<'EOF';
[^\s,;'`~@^()[\]{}"]
EOF

sub read_list {
    my ( $str, $end ) = @_;

    # print "read_list: /${$str}/$end/\n";
    my @lst;
    while () {
        ${$str} =~ s/ \A $separators //;
        ${$str} or die "expected '$end', got EOF";
        last if ( ${$str} =~ s/ \A $end // );
        push @lst, read_form($str);
    }
    return \@lst;
}

sub quote {
    my ( $quoter, @args ) = @_;

    # print "read_form: quote/$quoter/\n";
    return Mal::List->new( [ Mal::Symbol->new($quoter), @args ] );
}

sub read_form {
    my $str = shift;

    # print "read_form: /${$str}/\n";

    # Always skip initial separators.
    ${$str} =~ s/ \A $separators //;

    if ( ${$str} =~ s/ \A ' // ) {
        return quote( 'quote', read_form($str) );
    }
    if ( ${$str} =~ s/ \A ` // ) {
        return quote( 'quasiquote', read_form($str) );
    }
    if ( ${$str} =~ s/ \A ~ // ) {
        return quote( ${$str} =~ s/ \A @ // ? 'splice-unquote' : 'unquote',
            read_form($str) );
    }
    if ( ${$str} =~ s/ \A \^ // ) {
        my $meta = read_form($str);
        return quote( 'with-meta', read_form($str), $meta );
    }
    if ( ${$str} =~ s/ \A @ // ) {
        return quote( 'deref', read_form($str) );
    }
    if ( ${$str} =~ s/ \A [(] // ) {
        return Mal::List->new( read_list( $str, '\)' ) );
    }
    if ( ${$str} =~ s/ \A \[ // ) {
        return Mal::Vector->new( read_list( $str, '\]' ) );
    }
    if ( ${$str} =~ s/ \A [{] // ) {
        return Mal::HashMap->new( { @{ read_list( $str, '\}' ) } } );
    }
    if ( ${$str} =~ s/ \A ( -? \d+ ) // ) {
        return Mal::Integer->new($1);
    }
    if ( ${$str} =~ s/ \A " // ) {
        ${$str} =~ s/ \A ( (?: \\ . | [^\\"] )* ) " //
          or die 'expected ", got EOF';
        return Mal::String->new( $1 =~ s/ \\ (.) / $1 =~ tr|n|\n|r /ger );
    }
    if ( ${$str} =~ s/ \A : // ) {
        ${$str} =~ s/ \A ( $normal + ) //
          or die 'letters expected after a colon';
        return Mal::Keyword->new($1);
    }
    if ( ${$str} =~ s/ \A ( $normal+ ) // ) {
        if ( $1 eq 'nil' )   { return nil; }
        if ( $1 eq 'true' )  { return true; }
        if ( $1 eq 'false' ) { return false; }
        return Mal::Symbol->new($1);
    }
    if ( ${$str} =~ / \A [)\]}] / ) {
        die "unexpected '$1'";
    }
    die "Failed to parse '${$str}'";
}

sub read_str {
    my $str = shift;
    return read_form( \$str );
}

#print Dumper(read_str("123"));
#print Dumper(read_str("+"));
#print Dumper(read_str("\"abc\""));
#print Dumper(read_str("nil"));
#print Dumper(read_str("true"));
#print Dumper(read_str("false"));
#print Dumper(read_str("(+ 2 3)"));
#print Dumper(read_str("(foo 2 (3 4))"));

1;
