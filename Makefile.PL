
use strict;
use warnings;

use 5.008008;

use ExtUtils::MakeMaker 6.30;



my %WriteMakefileArgs = (
  "ABSTRACT" => "Type constraints for numbers",
  "AUTHOR" => "Brendan Byrd <BBYRD\@CPAN.org>",
  "BUILD_REQUIRES" => {},
  "CONFIGURE_REQUIRES" => {},
  "DISTNAME" => "Types-Numbers",
  "EXE_FILES" => [],
  "LICENSE" => "artistic_2",
  "NAME" => "Types::Numbers",
  "PREREQ_PM" => {
    "Data::Float" => "0.008",
    "Data::Integer" => "0.004",
    "Math::BigFloat" => "1.59",
    "Math::BigInt" => "1.88",
    "Type::Library" => "0.008",
    "Type::Tiny" => "0.001",
    "Types::Standard" => "0.001",
    "constant" => "1.16",
    "warnings" => "1.05_01"
  },
  "TEST_REQUIRES" => {
    "IO::Handle" => "1.27_02",
    "List::Util" => "1.20",
    "Test::CheckDeps" => "0.007",
    "Test::Exception" => "0.27",
    "Test::More" => "0.94",
    "Test::TypeTiny" => "0.001",
    "Type::Tiny" => 0,
    "base" => "2.14"
  },
  "VERSION" => "0.91_01",
  "test" => {
    "TESTS" => "t/*.t"
  }
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.63_03) } ) {
  my $tr = delete $WriteMakefileArgs{TEST_REQUIRES};
  my $br = $WriteMakefileArgs{BUILD_REQUIRES};
  for my $mod ( keys %$tr ) {
    if ( exists $br->{$mod} ) {
      $br->{$mod} = $tr->{$mod} if $tr->{$mod} > $br->{$mod};
    }
    else {
      $br->{$mod} = $tr->{$mod};
    }
  }
}

unless ( eval { ExtUtils::MakeMaker->VERSION(6.56) } ) {
  my $br = delete $WriteMakefileArgs{BUILD_REQUIRES};
  my $pp = $WriteMakefileArgs{PREREQ_PM};
  for my $mod ( keys %$br ) {
    if ( exists $pp->{$mod} ) {
      $pp->{$mod} = $br->{$mod} if $br->{$mod} > $pp->{$mod};
    }
    else {
      $pp->{$mod} = $br->{$mod};
    }
  }
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);



