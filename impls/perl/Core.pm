package Core;
use re '/msx';
use strict;
use warnings;

use English '-no_match_vars';
use Hash::Util  qw(fieldhash);
use Time::HiRes qw(time);

use Readline qw(mal_readline);
use Types    qw(equal_q thaw_key nil true false);
use Reader   qw(read_str);
use Printer  qw(pr_list);
use Interop  qw(pl_to_mal);

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
    print pr_list( q{ }, 1, @args ), "\n" or die $ERRNO;
    return nil;
}

sub println {
    my @args = @_;
    print pr_list( q{ }, 0, @args ), "\n" or die $ERRNO;
    return nil;
}

sub core_readline {
    my ($prompt) = @_;
    my $line = mal_readline( ${$prompt} );
    return defined $line ? Mal::String->new($line) : nil;
}

sub slurp {
    my ($filename) = @_;
    local $INPUT_RECORD_SEPARATOR = undef;
    open my $fh, q{<}, ${$filename} or die $ERRNO;
    my $data = <$fh>;
    close $fh or die $ERRNO;
    return Mal::String->new($data);
}

# Hash Map functions

sub assoc {
    my ( $src_hsh, @keys ) = @_;
    return Mal::HashMap->new( { %{$src_hsh}, @keys } );
}

sub dissoc {
    my ( $map, @keys ) = @_;
    my $new_hsh = { %{$map} };
    delete @{$new_hsh}{@keys};
    return Mal::HashMap->new($new_hsh);
}

sub get {
    my ( $hsh, $key ) = @_;
    return $hsh->{$key} // nil;
}

sub contains_q {
    my ( $hsh, $key ) = @_;
    return mal_bool( exists $hsh->{$key} );
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
    return $seq->[ ${$i} ] // die 'nth: index out of bounds';
}

sub first {
    my ($seq) = @_;
    return $seq->[0] // nil;
}

sub rest {
    my ($l) = @_;
    return Mal::List->new( [ @{$l}[ 1 .. $#{$l} ] ] );
}

sub apply {
    my ( $f, @args ) = @_;
    my $more_args = pop @args;
    return $f->( @args, @{$more_args} );
}

sub mal_map {
    my ( $f, $args ) = @_;
    return Mal::List->new( [ map { $f->($_) } @{$args} ] );
}

sub conj {
    my ( $seq, @items ) = @_;
    if ( $seq->isa('Mal::List') ) {
        return Mal::List->new( [ reverse(@items), @{$seq} ] );
    }
    else {
        return Mal::Vector->new( [ @{$seq}, @items ] );
    }
}

sub seq {
    my ($arg) = @_;
    if ( $arg->isa('Mal::List') and @{$arg} ) {
        return $arg;
    }
    if ( $arg->isa('Mal::Vector') and @{$arg} ) {
        return Mal::List->new( [ @{$arg} ] );
    }
    if ( $arg->isa('Mal::String') and length ${$arg} ) {
        return Mal::List->new(
            [ map { Mal::String->new($_) } split //, ${$arg} ] );
    }
    return nil;
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
sub swap_bang {
    my ( $atm, $f, @args ) = @_;
    return ${$atm} = $f->( ${$atm}, @args );
}

# Interop

# Force array context so that undef is a valid result.
sub pl_star {
    my ($perl) = @_;
    ## no critic (BuiltinFunctions::ProhibitStringyEval)
    my @result = eval ${$perl};
    ## use critic
    @result or die $EVAL_ERROR;
    return pl_to_mal( $result[0] );
}

sub mal_bool {
    my ($test) = @_;
    return $test ? true : false;
}

our %NS = (
    q{=}       => sub { mal_bool( equal_q( $_[0], $_[1] ) ) },
    'throw'    => sub { die $_[0] },
    'nil?'     => sub { mal_bool( $_[0]->isa('Mal::Nil') ) },
    'true?'    => sub { mal_bool( $_[0]->isa('Mal::True') ) },
    'false?'   => sub { mal_bool( $_[0]->isa('Mal::False') ) },
    'number?'  => sub { mal_bool( $_[0]->isa('Mal::Integer') ) },
    'symbol'   => sub { Mal::Symbol->new( ${ $_[0] } ) },
    'symbol?'  => sub { mal_bool( $_[0]->isa('Mal::Symbol') ) },
    'string?'  => sub { mal_bool( $_[0]->isa('Mal::String') ) },
    'keyword'  => sub { Mal::Keyword->new( ${ $_[0] } ) },
    'keyword?' => sub { mal_bool( $_[0]->isa('Mal::Keyword') ) },
    'fn?'      => sub { mal_bool( $_[0]->isa('Mal::Function') ) },
    'macro?'   => sub { mal_bool( $_[0]->isa('Mal::Macro') ) },

    'pr-str'      => \&pr_str,
    'str'         => \&str,
    'prn'         => \&prn,
    'println'     => \&println,
    'readline'    => \&core_readline,
    'read-string' => sub { read_str( ${ $_[0] } ) },
    'slurp'       => \&slurp,
    '<'           => sub { mal_bool( ${ $_[0] } < ${ $_[1] } ) },
    '<='          => sub { mal_bool( ${ $_[0] } <= ${ $_[1] } ) },
    '>'           => sub { mal_bool( ${ $_[0] } > ${ $_[1] } ) },
    '>='          => sub { mal_bool( ${ $_[0] } >= ${ $_[1] } ) },
    q{+}          => sub { Mal::Integer->new( ${ $_[0] } + ${ $_[1] } ) },
    q{-}          => sub { Mal::Integer->new( ${ $_[0] } - ${ $_[1] } ) },
    q{*}          => sub { Mal::Integer->new( ${ $_[0] } * ${ $_[1] } ) },
    q{/}          => sub { Mal::Integer->new( ${ $_[0] } / ${ $_[1] } ) },
    ## no critic (ValuesAndExpressions::ProhibitMagicNumbers)
    'time-ms' => sub { Mal::Integer->new( int( time() * 1000 ) ) },
    ## use critic

    'list'      => sub { Mal::List->new( \@_ ) },
    'list?'     => sub { mal_bool( $_[0]->isa('Mal::List') ) },
    'vector'    => sub { Mal::Vector->new( \@_ ) },
    'vector?'   => sub { mal_bool( $_[0]->isa('Mal::Vector') ) },
    'hash-map'  => sub { Mal::HashMap->new( {@_} ) },
    'map?'      => sub { mal_bool( $_[0]->isa('Mal::HashMap') ) },
    'assoc'     => \&assoc,
    'dissoc'    => \&dissoc,
    'get'       => \&get,
    'contains?' => \&contains_q,
    'keys'      => \&mal_keys,
    'vals'      => \&mal_vals,

    'sequential?' => sub { mal_bool( $_[0]->isa('Mal::Sequence') ) },
    'nth'         => \&nth,
    'first'       => \&first,
    'rest'        => \&rest,
    'cons'        => \&cons,
    'concat'      => \&concat,
    'vec'         => sub { Mal::Vector->new( [ @{ $_[0] } ] ) },
    'empty?'      => sub { mal_bool( not @{ $_[0] } ) },
    'count'       => sub { Mal::Integer->new( scalar @{ $_[0] } ) },
    'apply'       => \&apply,
    'map'         => \&mal_map,
    'conj'        => \&conj,
    'seq'         => \&seq,

    'with-meta' => \&with_meta,
    'meta'      => sub { $meta{ $_[0] } // nil },
    'atom'      => sub { Mal::Atom->new( $_[0] ) },
    'atom?'     => sub { mal_bool( $_[0]->isa('Mal::Atom') ) },
    'deref'     => sub { ${ $_[0] } },
    'reset!'    => sub { ${ $_[0] } = $_[1] },
    'swap!'     => \&swap_bang,

    'pl*' => \&pl_star,
);

1;
