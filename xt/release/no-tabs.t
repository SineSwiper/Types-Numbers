use strict;
use warnings;

# this test was generated with Dist::Zilla::Plugin::NoTabsTests 0.04

use Test::More 0.88;
use Test::NoTabs;

my @files = (
    'lib/Types/Numbers.pm'
);

notabs_ok($_) foreach @files;
done_testing;
