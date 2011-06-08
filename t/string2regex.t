use warnings;
use strict;
use Convert::Moji qw/make_regex/;
use Test::More tests => 1;
my $converter = Convert::Moji->new (['tr', 'a-m', 'n-z']);
my @bits = $converter->split_by_input_alphabet ("fabdibble");
#print "@bits\n";
my $converter2 =
    Convert::Moji->new (['table', {a => 'b', c => 'd', rad => 'dad'}]);
my @bits2 = $converter2->split_by_input_alphabet ("acradradcarad");
print "@bits2\n";
ok ($bits2[3] eq 'rad');
