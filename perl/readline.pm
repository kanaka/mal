# To get readline line editing functionality, please install
# Term::ReadLine::Gnu (GPL) or Term::ReadLine::Perl (GPL, Artistic)
# from CPAN.

package readline;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw( mal_readline );

use Term::ReadLine;

my $_rl = Term::ReadLine->new('Mal');
$_rl->ornaments(0);
#print "Using ReadLine implementation: " . $_rl->ReadLine() . "\n";
my $OUT = $_rl->OUT || \*STDOUT;
my $_history_loaded = 0;

sub mal_readline {
    my($prompt) = @_;
    my $line = undef;
    if (! $_history_loaded) {
        $_history_loaded = 1;
        # TODO: load history
    }

    if (defined ($line = $_rl->readline($prompt))) {
        $_rl->addhistory($line) if $line =~ /\S/;
        # TODO: save history
        return $line;
    } else {
        return undef;
    }
}
1;
