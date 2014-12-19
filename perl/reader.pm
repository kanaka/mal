package reader;
use feature qw(switch);
use strict;
use warnings FATAL => qw(all);
no if $] >= 5.018, warnings => "experimental::smartmatch";
use Exporter 'import';
our @EXPORT_OK = qw( read_str );

use types qw($nil $true $false _keyword _hash_map);

use Data::Dumper;

{
    package Reader;
    sub new  {
        my $class = shift;
        bless { position => 0, tokens => shift } => $class
    }
    sub next { my $self = shift; return $self->{tokens}[$self->{position}++] }
    sub peek { my $self = shift; return $self->{tokens}[$self->{position}] }
}

sub tokenize {
    my($str) = @_;
    my @tokens = $str =~ /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/g;
    return grep {! /^;|^$/} @tokens;
}

sub read_atom {
    my($rdr) = @_;
    my $token = $rdr->next();
    given ($token) {
        when(/^-?[0-9]+$/) { return Integer->new($token) }
        when(/^"/) {
            my $str = substr $token, 1, -1;
            $str =~ s/\\"/"/g;
            $str =~ s/\\n/\n/g;
            return String->new($str)
        }
        when(/^:/) { return _keyword(substr($token,1)) }
        when(/^nil$/) { return $nil }
        when(/^true$/) { return $true }
        when(/^false$/) { return $false }
        default { return Symbol->new($token) }
    }
}

sub read_list {
    my($rdr,$class,$start,$end) = @_;
    $start = $start || '(';
    $end = $end || ')';

    my $token = $rdr->next();
    my @lst = ();
    if ($token ne $start) {
        die "expected '$start'";
    }
    while (($token = $rdr->peek()) ne $end) {
        if (! defined $token) {
            die "expected '$end', got EOF";
        }
        push(@lst, read_form($rdr));
    }
    $rdr->next();
    if ($class eq 'List') {
        return List->new(\@lst);
    } elsif ($class eq 'Vector') {
        return Vector->new(\@lst);
    } else {
        return _hash_map(@lst);
    }
}

sub read_form {
    my($rdr) = @_;
    my $token = $rdr->peek();
    given ($token) {
        when("'") { $rdr->next(); List->new([Symbol->new('quote'),
                                             read_form($rdr)]) }
        when('`') { $rdr->next(); List->new([Symbol->new('quasiquote'),
                                             read_form($rdr)]) }
        when('~') { $rdr->next(); List->new([Symbol->new('unquote'),
                                             read_form($rdr)]) }
        when('~@') { $rdr->next(); List->new([Symbol->new('splice-unquote'),
                                              read_form($rdr)]) }
        when('^') { $rdr->next(); my $meta = read_form($rdr);
                    List->new([Symbol->new('with-meta'),
                               read_form($rdr), $meta]) }
        when('@') { $rdr->next(); List->new([Symbol->new('deref'),
                                              read_form($rdr)]) }

        when(')') { die "unexpected ')'" }
        when('(') { return read_list($rdr, 'List') }
        when(']') { die "unexpected ']'" }
        when('[') { return read_list($rdr, 'Vector', '[', ']') }
        when('}') { die "unexpected '}'" }
        when('{') { return read_list($rdr, 'HashMap', '{', '}') }
        default  { return read_atom($rdr) }
    }
}

sub read_str {
    my($str) = @_;
    my @tokens = tokenize($str);
    #print "tokens: " . Dumper(\@tokens);
    if (scalar(@tokens) == 0) { die BlankException->new(); }
    return read_form(Reader->new(\@tokens));
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
