use strict; use warnings;
package ReadLine;

BEGIN { $ENV{PERL_RL} = 'Gnu' }
use Term::ReadLine;

use Exporter 'import';

our @EXPORT = qw( readline );

my $history_file = "$ENV{HOME}/.mal_history";

my $tty;
{
    local @ENV{qw(HOME EDITOR)};
    local $^W;
    $tty = Term::ReadLine->new('Mal');
}

die "Please install Term::ReadLine::Gnu from CPAN\n"
    if $tty->ReadLine ne 'Term::ReadLine::Gnu';

sub readline {
    my ($prompt, $env) = @_;
    $tty->Attribs->{completion_function} = sub {
        my ($text, $line, $start) = @_;
        keys %{$env->{stash}}, qw(
            catch*
            def!
            defmacro!
            do
            false
            fn*
            if
            let*
            macroexpand
            nil
            quasiquote
            quasiquoteexpand
            try*
            quote
            true
        );
    };
    $tty->readline($prompt);
}

$tty->ReadHistory($history_file);

END {
    $tty->WriteHistory($history_file);
}

1;
