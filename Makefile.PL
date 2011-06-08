use warnings;
use strict;
use ExtUtils::MakeMaker;

my $pm = 'lib/Convert/Moji.pm';

WriteMakefile (
    NAME => 'Convert::Moji',
    VERSION_FROM => $pm,
    AUTHOR => 'Ben Bullock <bkb@cpan.org>',
    LICENSE => 'perl',
);
