package ReadLine;

use Mo;

BEGIN { $ENV{PERL_RL} = 'GNU' }

use Term::ReadLine;

use Exporter 'import';

our @EXPORT = qw( readline );

my $tty = Term::ReadLine->new('');

sub readline {
    my ($prompt) = (@_, '> ');
    $_ = $tty->readline($prompt);
    $tty->addhistory($_) if defined $_ and /\S/;
    return $_;
}


1;
