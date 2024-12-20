# To get readline line editing functionality, please install
# Term::ReadKey and either Term::ReadLine::Gnu (GPL) or
# Term::ReadLine::Perl (GPL, Artistic) from CPAN.

package Readline;
use re '/msx';
use strict;
use warnings;

use English '-no_match_vars';
use Term::ReadLine;

use Exporter 'import';
our @EXPORT_OK = qw( mal_readline set_rl_mode );

my $_rl = Term::ReadLine->new('Mal');
$_rl->ornaments(0);

#print "Using ReadLine implementation: " . $_rl->ReadLine() . "\n";
my $OUT             = $_rl->OUT || \*STDOUT;
my $_history_loaded = 0;

my $history_file = "$ENV{'HOME'}/.mal-history";

sub save_line {
    my ($line) = @_;
    open my $fh, '>>', $history_file or return;
    print {$fh} "$line\n" or die $ERRNO;
    close $fh             or die $ERRNO;
    return;
}

sub load_history {
    open my $fh, q{<}, $history_file or return;

    while ( my $line = <$fh> ) {
        chomp $line;
        $line =~ /\S/ or next;
        $_rl->addhistory($line);
    }

    close $fh or die $ERRNO;
    return;
}

my $rl_mode = 'terminal';

sub set_rl_mode {
    my ($mode) = @_;
    $rl_mode = $mode;
    return;
}

sub mal_readline {
    my ($prompt) = @_;
    my $line;
    if ( !$_history_loaded ) {
        $_history_loaded = 1;
        load_history();
    }

    if ( $rl_mode eq 'terminal' ) {
        $line = $_rl->readline($prompt);
    }
    else {
        print $prompt or die $ERRNO;
        $line = readline *STDIN;
    }
    if ($line) {
        chomp $line;
        if ($line) {
            save_line($line);
        }
    }
    return $line;
}

1;
