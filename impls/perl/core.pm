package core;
use strict;
use warnings;

use Hash::Util  qw(fieldhash);
use Time::HiRes qw(time);

use readline qw(mal_readline);
use types    qw(_equal_Q thaw_key $nil $true $false);
use reader   qw(read_str);
use printer  qw(pr_list);
use interop  qw(pl_to_mal);

use Exporter 'import';
our @EXPORT_OK = qw(%NS);

# String functions

sub pr_str {
    my @args = @_;
    return Mal::String->new( pr_list( q{ }, 1, @args ) );
}

sub str {
    my @args = @_;
    return Mal::String->new( pr_list( q{}, 0, @args ) );
}

sub prn {
    my @args = @_;
    print pr_list( q{ }, 1, @args ), "\n";
    return $nil;
}

sub println {
    my @args = @_;
    print pr_list( q{ }, 0, @args ), "\n";
    return $nil;
}

sub core_readline {
    my ($prompt) = @_;
    my $line = mal_readline( ${$prompt} );
    return defined $line ? Mal::String->new($line) : $nil;
}

sub slurp {
    my ($filename) = @_;
    use autodie;
    open my $fh, q{<}, ${$filename};
    my $data = do { local $/; <$fh> };
    close $fh;
    return Mal::String->new($data);
}

# Hash Map functions

sub assoc {
    my ( $src_hsh, @keys ) = @_;
    return Mal::HashMap->new( { %$src_hsh, @keys } );
}

sub dissoc {
    my ( $map, @keys ) = @_;
    my $new_hsh = { %{$map} };
    delete @{$new_hsh}{@keys};
    return Mal::HashMap->new($new_hsh);
}

sub get {
    my ( $hsh, $key ) = @_;
    return $hsh->{$key} // $nil;
}

sub contains_Q {
    my ( $hsh, $key ) = @_;
    return ( exists $hsh->{$key} ) ? $true : $false;
}

sub mal_keys {
    my ($map) = @_;
    return Mal::List->new( [ map { thaw_key($_) } keys %{$map} ] );
}

sub mal_vals {
    my ($map) = @_;
    return Mal::List->new( [ values %{$map} ] );
}

# Sequence functions

sub cons {
    my ( $a, $b ) = @_;
    return Mal::List->new( [ $a, @{$b} ] );
}

sub concat {
    my @args = @_;
    return Mal::List->new( [ map { @{$_} } @args ] );
}

sub nth {
    my ( $seq, $i ) = @_;
    return $seq->[$i] // die "nth: index out of bounds";
}

sub first {
    my ($seq) = @_;
    return $seq->[0] // $nil;
}

sub apply {
    my ( $f, @args ) = @_;
    my $more_args = pop @args;
    return &{$f}( @args, @{$more_args} );
}

sub mal_map {
    my ( $f, $args ) = @_;
    return Mal::List->new( [ map { &$f($_) } @{$args} ] );
}

sub conj {
    my ( $seq, @items ) = @_;
    my $new_seq = $seq->clone;
    if ( $new_seq->isa('Mal::List') ) {
        unshift @$new_seq, reverse @items;
    }
    else {
        push @$new_seq, @items;
    }
    return $new_seq;
}

sub seq {
    my ($arg) = @_;
    if ( $arg eq $nil ) {
        return $nil;
    }
    elsif ( $arg->isa('Mal::List') ) {
        return $nil unless @$arg;
        return $arg;
    }
    elsif ( $arg->isa('Mal::Vector') ) {
        return $nil unless @$arg;
        return Mal::List->new( [@$arg] );
    }
    elsif ( $arg->isa('Mal::String') ) {
        return $nil if length($$arg) == 0;
        my @chars = map { Mal::String->new($_) } split( //, $$arg );
        return Mal::List->new( \@chars );
    }
    else {
        die "seq requires list or vector or string or nil";
    }
}

fieldhash my %meta;

# Metadata functions
sub with_meta {
    my ( $old, $new_meta ) = @_;
    my $new_obj = $old->clone;
    $meta{$new_obj} = $new_meta;
    return $new_obj;
}

# Atom functions
sub swap_BANG {
    my ( $atm, $f, @args ) = @_;
    return $$atm = &$f( $$atm, @args );
}

# Interop

# Force array context so that undef is a valid result.
sub pl_STAR {
    my ($perl) = @_;
    ## no critic (BuiltinFunctions::ProhibitStringyEval)
    my @result = eval ${$perl};
    ## use critic
    @result or die $@;
    return pl_to_mal( $result[0] );
}

our %NS = (
    '='        => sub { _equal_Q( $_[0], $_[1] ) ? $true : $false },
    'throw'    => sub { die $_[0] },
    'nil?'     => sub { $_[0] eq $nil              ? $true : $false },
    'true?'    => sub { $_[0] eq $true             ? $true : $false },
    'false?'   => sub { $_[0] eq $false            ? $true : $false },
    'number?'  => sub { $_[0]->isa('Mal::Integer') ? $true : $false },
    'symbol'   => sub { Mal::Symbol->new( ${ $_[0] } ) },
    'symbol?'  => sub { $_[0]->isa('Mal::Symbol') ? $true : $false },
    'string?'  => sub { $_[0]->isa('Mal::String') ? $true : $false },
    'keyword'  => sub { Mal::Keyword->new( ${ $_[0] } ) },
    'keyword?' => sub { $_[0]->isa('Mal::Keyword')  ? $true : $false },
    'fn?'      => sub { $_[0]->isa('Mal::Function') ? $true : $false },
    'macro?'   => sub { $_[0]->isa('Mal::Macro')    ? $true : $false },

    'pr-str'      => \&pr_str,
    'str'         => \&str,
    'prn'         => \&prn,
    'println'     => \&println,
    'readline'    => \&core_readline,
    'read-string' => sub { read_str( ${ $_[0] } ) },
    'slurp'       => \&slurp,
    '<'           => sub { ${ $_[0] } < ${ $_[1] }  ? $true : $false },
    '<='          => sub { ${ $_[0] } <= ${ $_[1] } ? $true : $false },
    '>'           => sub { ${ $_[0] } > ${ $_[1] }  ? $true : $false },
    '>='          => sub { ${ $_[0] } >= ${ $_[1] } ? $true : $false },
    '+'           => sub { Mal::Integer->new( ${ $_[0] } + ${ $_[1] } ) },
    '-'           => sub { Mal::Integer->new( ${ $_[0] } - ${ $_[1] } ) },
    '*'           => sub { Mal::Integer->new( ${ $_[0] } * ${ $_[1] } ) },
    '/'           => sub { Mal::Integer->new( ${ $_[0] } / ${ $_[1] } ) },
    'time-ms'     => sub { Mal::Integer->new( int( time() * 1000 ) ) },

    'list'      => sub { Mal::List->new( \@_ ) },
    'list?'     => sub { $_[0]->isa('Mal::List') ? $true : $false },
    'vector'    => sub { Mal::Vector->new( \@_ ) },
    'vector?'   => sub { $_[0]->isa('Mal::Vector') ? $true : $false },
    'hash-map'  => sub { Mal::HashMap->new( \@_ ) },
    'map?'      => sub { $_[0]->isa('Mal::HashMap') ? $true : $false },
    'assoc'     => \&assoc,
    'dissoc'    => \&dissoc,
    'get'       => \&get,
    'contains?' => \&contains_Q,
    'keys'      => \&mal_keys,
    'vals'      => \&mal_vals,

    'sequential?' => sub { $_[0]->isa('Mal::Sequence') ? $true : $false },
    'nth'         => sub { nth( $_[0], ${ $_[1] } ) },
    'first'       => \&first,
    'rest'        => sub { $_[0]->rest() },
    'cons'        => \&cons,
    'concat'      => \&concat,
    'vec'         => sub { Mal::Vector->new( [ @{ $_[0] } ] ) },
    'empty?'      => sub { @{ $_[0] } ? $false : $true },
    'count'       => sub { Mal::Integer->new( scalar( @{ $_[0] } ) ) },
    'apply'       => \&apply,
    'map'         => \&mal_map,
    'conj'        => \&conj,
    'seq'         => \&seq,

    'with-meta' => \&with_meta,
    'meta'      => sub { $meta{ $_[0] } // $nil },
    'atom'      => sub { Mal::Atom->new( $_[0] ) },
    'atom?'     => sub { $_[0]->isa('Mal::Atom') ? $true : $false },
    'deref'     => sub { ${ $_[0] } },
    'reset!'    => sub { ${ $_[0] } = $_[1] },
    'swap!'     => \&swap_BANG,

    'pl*' => \&pl_STAR,
);

1;
