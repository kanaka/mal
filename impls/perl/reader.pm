package reader;
use strict;
use warnings;
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);

use Exporter 'import';
our @EXPORT_OK = qw( read_str );

use types qw($nil $true $false);

## no critic (RegularExpressions::RequireExtendedFormatting)
## no critic (RegularExpressions::ProhibitComplexRegexes)
## no critic (RegularExpressions::RequireLineBoundaryMatching)
## no critic (RegularExpressions::RequireDotMatchAnything)

{

    ## no critic (ProhibitMultiplePackages)
    package Mal::Reader;
    ## use critic

    sub new {
        my $class = shift;
        return bless { position => 0, tokens => shift } => $class;
    }

    sub next_ {
        my $self = shift;
        return $self->{tokens}[ $self->{position}++ ];
    }
    sub peek { my $self = shift; return $self->{tokens}[ $self->{position} ] }
}

sub tokenize {
    my ($str) = @_;
    my @tokens = $str =~
      /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/g;
    return grep { !/^;|^$/ } @tokens;
}

sub quote {
    my ( $quoter, @args ) = @_;

    # print "read_form: quote/$quoter/\n";
    return Mal::List->new( [ Mal::Symbol->new($quoter), @args ] );
}

sub read_atom {
    my ($rdr) = @_;
    my $token = $rdr->next_();
    given ($token) {
        when (/^-?[0-9]+$/) { return Mal::Integer->new($token) }
        when (/^"((?:\\.|[^\\"])*)"$/) {
            return Mal::String->new( $1 =~ s/\\(.)/$1 =~ tr|n|\n|r/ger );
        }
        when (/^"/) {
            die "expected '\"', got EOF";
        }
        when (/^:(.*)/) { return Mal::Keyword->new($1) }
        when ('nil')    { return $nil }
        when ('true')   { return $true }
        when ('false')  { return $false }
        default         { return Mal::Symbol->new($token) }
    }
}

sub read_list {
    my ( $rdr, $class, $start, $end ) = @_;
    $start = $start // '(';
    $end   = $end   // ')';

    my $token = $rdr->next_();
    my @lst   = ();
    if ( $token ne $start ) {
        die "expected '$start'";
    }
    while (1) {
        $token = $rdr->peek();
        if ( !defined($token) ) {
            die "expected '$end', got EOF";
        }
        last if ( $token eq $end );
        push( @lst, read_form($rdr) );
    }
    $rdr->next_();
    return $class->new( \@lst );
}

sub read_form {
    my ($rdr) = @_;
    my $token = $rdr->peek();
    given ($token) {
        when (q{'}) {
            $rdr->next_();
            return quote( 'quote', read_form($rdr) );
        }
        when (q{`}) {
            $rdr->next_();
            return quote( 'quasiquote', read_form($rdr) );
        }
        when (q{~}) {
            $rdr->next_();
            return quote( 'unquote', read_form($rdr) );
        }
        when (q{~@}) {
            $rdr->next_();
            return quote( 'splice-unquote', read_form($rdr) );
        }
        when (q{^}) {
            $rdr->next_();
            my $meta = read_form($rdr);
            return quote( 'with-meta', read_form($rdr), $meta );
        }
        when (q{@}) {
            $rdr->next_();
            return quote( 'deref', read_form($rdr) );
        }

        when (')') { die "unexpected ')'" }
        when ('(') { return read_list( $rdr, 'Mal::List' ) }
        when (']') { die "unexpected ']'" }
        when ('[') { return read_list( $rdr, 'Mal::Vector', '[', ']' ) }
        when ('}') { die "unexpected '}'" }
        when ('{') { return read_list( $rdr, 'Mal::HashMap', '{', '}' ) }
        default    { return read_atom($rdr) }
    }
}

sub read_str {
    my ($str) = @_;
    my @tokens = tokenize($str);

    #print "tokens: " . Dumper(\@tokens);
    if ( scalar(@tokens) == 0 ) { die Mal::BlankException->new(); }
    return read_form( Mal::Reader->new( \@tokens ) );
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
