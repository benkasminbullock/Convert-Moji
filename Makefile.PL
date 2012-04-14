use warnings;
use strict;
use ExtUtils::MakeMaker;

my $pm = 'lib/Convert/Moji.pm';

WriteMakefile (
    NAME => 'Convert::Moji',
    VERSION_FROM => $pm,
    ABSTRACT_FROM => $pm,
    AUTHOR => 'Ben Bullock <bkb@cpan.org>',
    LICENSE => 'perl',
    META_MERGE => {
        resources => {
            repository => '',
            bugtracker => '',
        },
    },
);
