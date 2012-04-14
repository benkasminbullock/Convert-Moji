#!/home/ben/software/install/bin/perl
use warnings;
use strict;
use Getopt::Long;

GetOptions (
    "clean" => \my $clean,
);
if ($clean) {
    clean ();
}
else {
    system ("perl Makefile.PL > /dev/null;make > /dev/null;make test");
}

exit;

sub clean
{
    if (-f "Makefile") {
        system ("make clean > /dev/null");
    }
    if (-f "Makefile.old") {
        unlink "Makefile.old" or die $!;
    }
}
