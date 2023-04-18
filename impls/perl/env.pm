use strict;
use warnings;

use Exporter 'import';

{

    package Mal::Env;
    use Data::Dumper;

    sub new {
        my ( $class, $outer, $binds, $exprs ) = @_;
        my $data = { __outer__ => $outer };
        if ($binds) {
            my @expr = @$exprs;
            foreach my $bind (@$binds) {
                if ( $$bind eq "&" ) {

                    # variable length arguments
                    @expr = ( Mal::List->new( [@expr] ) );
                    next;
                }
                $data->{$$bind} = shift @expr;
            }
        }
        bless $data => $class;
    }

    sub get {
        my ( $self, $key ) = @_;
        if    ( exists $self->{$key} ) { return $self->{$key}; }
        elsif ( $self->{__outer__} )   { return $self->{__outer__}->get($key); }
        else                           { return undef; }
    }

    sub set {
        my ( $self, $key, $value ) = @_;
        $self->{$key} = $value;
        return $value;
    }
}

#my $e1 = Mal::Env->new();
#print Dumper($e1);
#
#my $e2 = Mal::Env->new();
#$e2->set('abc', 123);
#$e2->set('def', 456);
#print Dumper($e2);
#
#my $e3 = Mal::Env->new($e2);
#$e3->set('abc', 789);
#$e3->set('ghi', 1024);
#print Dumper($e3);
#
#print Dumper($e3->get('abc'));
#print Dumper($e3->get('def'));

1;
