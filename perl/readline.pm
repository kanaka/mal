# To get readline line editing functionality, please install
# Term::ReadKey and either Term::ReadLine::Gnu (GPL) or
# Term::ReadLine::Perl (GPL, Artistic) from CPAN.

package readline;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw( mal_readline set_rl_mode );

use Term::ReadLine;

my $_rl = Term::ReadLine->new('Mal');
$_rl->ornaments(0);
#print "Using ReadLine implementation: " . $_rl->ReadLine() . "\n";
my $OUT = $_rl->OUT || \*STDOUT;
my $_history_loaded = 0;

my $history_file = $ENV{"HOME"} . "/.mal-history";

sub save_line {
    my ($line) = @_;
    open(my $fh, '>>', $history_file) or return;
    say $fh $line;
    close $fh;
}

sub load_history {
    open my $fh, $history_file or return;

    while(my $line = <$fh>)  {   
        chomp $line;
        $_rl->addhistory($line) if $line =~ /\S/;
    }

    close $fh;
}

my $rl_mode = "terminal";

sub set_rl_mode {
    my($mode) = @_;
    $rl_mode = $mode;
}

sub mal_readline {
    my($prompt) = @_;
    my $line = undef;
    if (! $_history_loaded) {
        $_history_loaded = 1;
        load_history();
    }

    if ($rl_mode eq "terminal") {
        if (defined ($line = $_rl->readline($prompt))) {
            save_line($line);
            chomp $line;
            return $line;
        } else {
            return undef;
        }
    } else {
        print "$prompt";
        if (defined ($line = readline(*STDIN))) {
            save_line($line);
            chomp($line);
            return $line;
        } else {
            return undef;
        }
    }
}
1;
