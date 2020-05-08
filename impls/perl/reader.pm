package reader;
use strict;
use warnings;
no if $] >= 5.018, warnings => "experimental::smartmatch";
use feature qw(switch);

use Exporter 'import';
our @EXPORT_OK = qw( read_str );

use types qw($nil $true $false);

use Data::Dumper;

{
    package Mal::Reader;
    sub new  {
        my $class = shift;
        bless { position => 0, tokens => shift } => $class
    }
    sub next { my $self = shift; return $self->{tokens}[$self->{position}++] }
    sub peek { my $self = shift; return $self->{tokens}[$self->{position}] }
}

sub tokenize {
    my($str) = @_;
    my @tokens = $str =~ /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/g;
    return grep {! /^;|^$/} @tokens;
}

sub read_atom {
    my($rdr) = @_;
    my $token = $rdr->next();
    given ($token) {
        when(/^-?[0-9]+$/) { return Mal::Integer->new($token) }
        when(/^"((?:\\.|[^\\"])*)"$/) {
            return Mal::String->new($1 =~ s/\\(.)/$1 =~ tr|n|\n|r/ger);
        }
        when(/^"/) {
            die "expected '\"', got EOF";
        }
        when(/^:/) { return Mal::Keyword->new($') }
        when('nil') { return $nil }
        when('true') { return $true }
        when('false') { return $false }
        default { return Mal::Symbol->new($token) }
    }
}

sub read_list {
    my($rdr,$class,$start,$end) = @_;
    $start = $start // '(';
    $end = $end // ')';

    my $token = $rdr->next();
    my @lst = ();
    if ($token ne $start) {
        die "expected '$start'";
    }
    while (1) {
        $token = $rdr->peek();
        if (! defined($token)) {
            die "expected '$end', got EOF";
        }
        last if ($token eq $end);
        push(@lst, read_form($rdr));
    }
    $rdr->next();
    return $class->new(\@lst);
}

sub read_form {
    my($rdr) = @_;
    my $token = $rdr->peek();
    given ($token) {
        when("'") { $rdr->next(); Mal::List->new([Mal::Symbol->new('quote'),
						  read_form($rdr)]) }
        when('`') { $rdr->next();
		    Mal::List->new([Mal::Symbol->new('quasiquote'),
				    read_form($rdr)]) }
        when('~') { $rdr->next(); Mal::List->new([Mal::Symbol->new('unquote'),
						  read_form($rdr)]) }
        when('~@') { $rdr->next();
		     Mal::List->new([Mal::Symbol->new('splice-unquote'),
				     read_form($rdr)]) }
        when('^') { $rdr->next(); my $meta = read_form($rdr);
                    Mal::List->new([Mal::Symbol->new('with-meta'),
				    read_form($rdr), $meta]) }
        when('@') { $rdr->next(); Mal::List->new([Mal::Symbol->new('deref'),
						  read_form($rdr)]) }

        when(')') { die "unexpected ')'" }
        when('(') { return read_list($rdr, 'Mal::List') }
        when(']') { die "unexpected ']'" }
        when('[') { return read_list($rdr, 'Mal::Vector', '[', ']') }
        when('}') { die "unexpected '}'" }
        when('{') { return read_list($rdr, 'Mal::HashMap', '{', '}') }
        default  { return read_atom($rdr) }
    }
}

sub read_str {
    my($str) = @_;
    my @tokens = tokenize($str);
    #print "tokens: " . Dumper(\@tokens);
    if (scalar(@tokens) == 0) { die Mal::BlankException->new(); }
    return read_form(Mal::Reader->new(\@tokens));
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
