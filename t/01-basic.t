use strict;
use warnings;

use Test::More;
use Test::Exception;
use Test::TypeTiny ();

use Types::Numbers ':all';

use Scalar::Util 'blessed';
use Data::Float;
use Math::BigInt;
use Math::BigFloat;

# configure some basic big number stuff
Math::BigInt  ->config({
   upgrade    => 'Math::BigFloat',
   round_mode => 'common',
   trap_nan   => 0,
   trap_inf   => 0,
});
Math::BigFloat->config({
   round_mode => 'common',
   trap_nan   => 0,
   trap_inf   => 0,
});

my $bigtwo = Math::BigInt->new(2);
my $bigten = Math::BigInt->new(10);

sub numbers_test {
   my ($val, $type, $is_pass) = @_;

   my $msg = sprintf("%s: %s %s",
      $type->name,
      ($is_pass ? 'accepts' : 'rejects'),
      (blessed $val ? "$val (blessed)" : $val),
   );
   $is_pass ?
      Test::TypeTiny::should_pass($val, $type, $msg) :
      Test::TypeTiny::should_fail($val, $type, $msg)
   ;
}

# NumLike
# NumRange
# IntLike
# BlessedNum
# NaN
# Inf
# RealNum
#
# PerlSafeInt
# BlessedInt
# SignedInt
# UnsignedInt
#
# BlessedFloat
# FloatBinary
# FloatDecimal
# FixedBinary
# FixedDecimal
#
# Char

### TODO: Char and param types ###
my @types = (
   NumLike, NumRange, IntLike, BlessedNum, NaN, Inf, RealNum,
   PerlSafeInt, BlessedInt, SignedInt, UnsignedInt,
   BlessedFloat, FloatBinary, FloatDecimal, FixedBinary, FixedDecimal
);

my $pass_types = {
   NumLike      => [qw( perl blessed uint sint float nan inf )],
   NumRange     => [qw( perl blessed uint sint float nan inf )],
   IntLike      => [qw( perl blessed uint sint               )],
   BlessedNum   => [qw(      blessed uint sint float nan inf )],
   NaN          => [qw( perl blessed                 nan     )],
   Inf          => [qw( perl blessed                     inf )],
   RealNum      => [qw( perl blessed uint sint float         )],

   PerlSafeInt  => [qw( perl         uint sint               )],
   BlessedInt   => [qw(      blessed uint sint               )],
   SignedInt    => [qw( perl blessed uint sint float nan inf )],
   UnsignedInt  => [qw( perl blessed uint      float nan inf )],

   BlessedFloat => [qw(      blessed uint sint float nan inf )],
   FloatBinary  => [qw( perl blessed uint sint float nan inf )],
   FloatDecimal => [qw( perl blessed uint sint float nan inf )],
   FixedBinary  => [qw( perl blessed uint sint float nan inf )],
   FixedDecimal => [qw( perl blessed uint sint float nan inf )],
};

my $B1   = Math::BigFloat->bone();   # everything else can be overloaded
my $B0   = Math::BigFloat->bzero();  # ...well, almost...
my $B_1  = -$B1;           # -1
my $B05  = $B1 / 2;        # +0.5
my $B15  = $B1 + $B05;     # +1.5
my $B_25 = -($B15 + $B1);  # -2.5

my $nan  = Data::Float::nan;
my $pinf = Data::Float::pos_infinity;
my $ninf = Data::Float::neg_infinity;

my $Bnan  = Math::BigFloat->bnan();
my $Bpinf = Math::BigFloat->binf('+');
my $Bninf = Math::BigFloat->binf('-');

foreach my $type (@types) {
   my $name = $type->name;
   my $should_pass = { map { $_ => 1 } @{ $pass_types->{$name} } };
   my $is_pass;

   subtest $name => sub {
      ### TODO: ok_subtype($type, @subtypes)

      numbers_test('ABC', $type, 0);

      $is_pass = $should_pass->{perl} && $should_pass->{uint};
      numbers_test(    0, $type, $is_pass);
      numbers_test(    1, $type, $is_pass);

      $is_pass = $should_pass->{perl} && $should_pass->{sint};
      numbers_test(   -1, $type, $is_pass);

      $is_pass = $should_pass->{perl} && $should_pass->{float};
      numbers_test(  0.5, $type, $is_pass);
      numbers_test( -2.5, $type, $is_pass);

      $is_pass = $should_pass->{perl} && $should_pass->{nan};
      numbers_test( $nan, $type, $is_pass);

      $is_pass = $should_pass->{perl} && $should_pass->{inf};
      numbers_test($pinf, $type, $is_pass);
      numbers_test($ninf, $type, $is_pass);

      $is_pass = $should_pass->{blessed} && $should_pass->{uint};
      numbers_test(  $B0, $type, $is_pass);
      numbers_test(  $B1, $type, $is_pass);

      $is_pass = $should_pass->{blessed} && $should_pass->{sint};
      numbers_test( $B_1, $type, $is_pass);

      $is_pass = $should_pass->{blessed} && $should_pass->{float};
      numbers_test( $B05, $type, $is_pass);
      numbers_test($B_25, $type, $is_pass);

      $is_pass = $should_pass->{blessed} && $should_pass->{nan};
      numbers_test($Bnan, $type, $is_pass);

      $is_pass = $should_pass->{blessed} && $should_pass->{inf};
      numbers_test($Bpinf, $type, $is_pass);
      numbers_test($Bninf, $type, $is_pass);

      #if ($name ~~ $types->{unsigned}) {
      #    dies_ok { $obj->$sub($n_1)  } "$name: Unsigned rejects -1";
      #    dies_ok { $obj->$sub($n05)  } "$name: Unsigned rejects  0.5";
      #    dies_ok { $obj->$sub($n15)  } "$name: Unsigned rejects  1.5";
      #    dies_ok { $obj->$sub($n_25) } "$name: Unsigned rejects -2.5";
      #    dies_ok { $obj->$sub($nan)  } "$name: Unsigned rejects  NaN";
      #    dies_ok { $obj->$sub($pinf) } "$name: Unsigned rejects +inf";
      #    dies_ok { $obj->$sub($ninf) } "$name: Unsigned rejects -inf";
      #}
      #if ($name ~~ $types->{signed}) {
      #   lives_ok { $obj->$sub($n_1)  } "$name: Signed accepts -1";
      #    dies_ok { $obj->$sub($n05)  } "$name: Signed rejects  0.5";
      #    dies_ok { $obj->$sub($n15)  } "$name: Signed rejects  1.5";
      #    dies_ok { $obj->$sub($n_25) } "$name: Signed rejects -2.5";
      #    dies_ok { $obj->$sub($nan)  } "$name: Signed rejects  NaN";
      #    dies_ok { $obj->$sub($pinf) } "$name: Signed rejects +inf";
      #    dies_ok { $obj->$sub($ninf) } "$name: Signed rejects -inf";
      #}
      #if ($name ~~ $types->{money}) {
      #   lives_ok { $obj->$sub($n_1)  } "$name: Money accepts -1";
      #   lives_ok { $obj->$sub($n05)  } "$name: Money accepts  0.5";
      #   lives_ok { $obj->$sub($n15)  } "$name: Money accepts  1.5";
      #   lives_ok { $obj->$sub($n_25) } "$name: Money accepts -2.5";
      #
      #    ### XXX: This behavior is undefined... ###
      #    # dies_ok { $obj->$sub($nan)  } "$name: Money rejects  NaN";
      #    # dies_ok { $obj->$sub($pinf) } "$name: Money rejects +inf";
      #    # dies_ok { $obj->$sub($ninf) } "$name: Money rejects -inf";
      #}
      #if ($name ~~ $types->{float}) {
      #   lives_ok { $obj->$sub($n_1)  } "$name: Float accepts -1";
      #   lives_ok { $obj->$sub($n05)  } "$name: Float accepts  0.5";
      #   lives_ok { $obj->$sub($n15)  } "$name: Float accepts  1.5";
      #   lives_ok { $obj->$sub($n_25) } "$name: Float accepts -2.5";
      #   lives_ok { $obj->$sub($nan)  } "$name: Float accepts  NaN";
      #   lives_ok { $obj->$sub($pinf) } "$name: Float accepts +inf";
      #   lives_ok { $obj->$sub($ninf) } "$name: Float accepts -inf";
      #}
      #if ($name ~~ $types->{decimal}) {
      #   lives_ok { $obj->$sub($n_1)  } "$name: Decimal accepts -1";
      #   lives_ok { $obj->$sub($n05)  } "$name: Decimal accepts  0.5";
      #   lives_ok { $obj->$sub($n15)  } "$name: Decimal accepts  1.5";
      #   lives_ok { $obj->$sub($n_25) } "$name: Decimal accepts -2.5";
      #   lives_ok { $obj->$sub($nan)  } "$name: Decimal accepts  NaN";
      #   lives_ok { $obj->$sub($pinf) } "$name: Decimal accepts +inf";
      #   lives_ok { $obj->$sub($ninf) } "$name: Decimal accepts -inf";
      #}
      #if ($name ~~ $types->{char}) {
      #    dies_ok { $obj->$sub($n_1)  } "$name: Char rejects -1";
      #    dies_ok { $obj->$sub($n05)  } "$name: Char rejects  0.5";
      #    dies_ok { $obj->$sub($n15)  } "$name: Char rejects  1.5";
      #    dies_ok { $obj->$sub($n_25) } "$name: Char rejects -2.5";
      #    dies_ok { $obj->$sub($nan)  } "$name: Char rejects  NaN";
      #    dies_ok { $obj->$sub($pinf) } "$name: Char rejects +inf";
      #    dies_ok { $obj->$sub($ninf) } "$name: Char rejects -inf";
      #   lives_ok { $obj->$sub('A')   } "$name: Char accepts 'A'";
      #}
      #
      ## Specific limits
      #
      ## (trying to minimize the level of automation while still keep some sanity...)
      #foreach my $bits (4,8,16,24,32,64,128) {
      #   next unless ($name ~~ $types->{'int'.$bits} || $name ~~ $types->{'uint'.$bits});
      #   my $spos = $bigtwo->copy ** ($bits-1) - 1;  # 8-bit =  127
      #   my $sneg = -1 - $spos;                      # 8-bit = -128
      #   my $upos = $bigtwo->copy ** $bits - 1;      # 8-bit =  255
      #
      #   if ($name ~~ $types->{'int'.$bits}) {
      #      lives_ok { $obj->$sub($spos+0) } "$name: $bits-bit Int accepts $spos+0 (scalar)";
      #      lives_ok { $obj->$sub($sneg-0) } "$name: $bits-bit Int accepts $sneg-0 (scalar)";
      #       dies_ok { $obj->$sub($upos+0) } "$name: $bits-bit Int rejects $upos+0 (scalar)";
      #       dies_ok { $obj->$sub($spos+1) } "$name: $bits-bit Int rejects $spos+1 (scalar)";
      #       dies_ok { $obj->$sub($sneg-1) } "$name: $bits-bit Int rejects $sneg-1 (scalar)";
      #       dies_ok { $obj->$sub($upos+1) } "$name: $bits-bit Int rejects $upos+1 (scalar)";
      #   }
      #   if ($name ~~ $types->{'uint'.$bits}) {
      #      lives_ok { $obj->$sub($spos+0) } "$name: $bits-bit UInt accepts $spos+0 (scalar)";
      #       dies_ok { $obj->$sub($sneg-0) } "$name: $bits-bit UInt rejects $sneg-0 (scalar)";
      #      lives_ok { $obj->$sub($upos+0) } "$name: $bits-bit UInt accepts $upos+0 (scalar)";
      #      lives_ok { $obj->$sub($spos+1) } "$name: $bits-bit UInt accepts $spos+1 (scalar)";
      #       dies_ok { $obj->$sub($sneg-1) } "$name: $bits-bit UInt rejects $sneg-1 (scalar)";
      #       dies_ok { $obj->$sub($upos+1) } "$name: $bits-bit UInt rejects $upos+1 (scalar)";
      #   }
      #
      #   # classes only for above 32-bit, just to be safe
      #   if ($bits <= 32) {
      #      $spos = 2 ** ($bits-1) - 1;  # 8-bit =  127
      #      $sneg = -1 - $spos;          # 8-bit = -128
      #      $upos = 2 ** $bits - 1;      # 8-bit =  255
      #
      #      if ($name ~~ $types->{'int'.$bits}) {
      #         lives_ok { $obj->$sub($spos+0) } "$name: $bits-bit Int accepts $spos+0 (BigInt)";
      #         lives_ok { $obj->$sub($sneg-0) } "$name: $bits-bit Int accepts $sneg-0 (BigInt)";
      #          dies_ok { $obj->$sub($upos+0) } "$name: $bits-bit Int rejects $upos+0 (BigInt)";
      #          dies_ok { $obj->$sub($spos+1) } "$name: $bits-bit Int rejects $spos+1 (BigInt)";
      #          dies_ok { $obj->$sub($sneg-1) } "$name: $bits-bit Int rejects $sneg-1 (BigInt)";
      #          dies_ok { $obj->$sub($upos+1) } "$name: $bits-bit Int rejects $upos+1 (BigInt)";
      #      }
      #      if ($name ~~ $types->{'uint'.$bits}) {
      #         lives_ok { $obj->$sub($spos+0) } "$name: $bits-bit UInt accepts $spos+0 (BigInt)";
      #          dies_ok { $obj->$sub($sneg-0) } "$name: $bits-bit UInt rejects $sneg-0 (BigInt)";
      #         lives_ok { $obj->$sub($upos+0) } "$name: $bits-bit UInt accepts $upos+0 (BigInt)";
      #         lives_ok { $obj->$sub($spos+1) } "$name: $bits-bit UInt accepts $spos+1 (BigInt)";
      #          dies_ok { $obj->$sub($sneg-1) } "$name: $bits-bit UInt rejects $sneg-1 (BigInt)";
      #          dies_ok { $obj->$sub($upos+1) } "$name: $bits-bit UInt rejects $upos+1 (BigInt)";
      #      }
      #   }
      #}
      #foreach my $bits (32,64,128) {
      #   next unless ($name ~~ $types->{'money'.$bits});
      #   my $pos = $bigtwo->copy ** ($bits-1) - 1;
      #   my $neg = -1 - $pos;
      #   my $s   = 10 ** -($bits > 64 ? 6 : 4);
      #
      #   $pos = Math::BigFloat->new($pos);
      #   $neg = Math::BigFloat->new($neg);
      #
      #   $pos *= $s;
      #   $neg *= $s;
      #
      #   lives_ok { $obj->$sub($pos+0 ) } "$name: $bits-bit Money accepts $pos+0 (BigFloat)";
      #   lives_ok { $obj->$sub($neg+0 ) } "$name: $bits-bit Money accepts $neg+0 (BigFloat)";
      #    dies_ok { $obj->$sub($pos+$s) } "$name: $bits-bit Money rejects $pos+$s (BigFloat)";
      #    dies_ok { $obj->$sub($neg-$s) } "$name: $bits-bit Money rejects $neg-$s (BigFloat)";
      #}
      #
      ## I hate copying module code for this, but I don't have much of a choice here...
      #foreach my $args (qw(16_4 16_5 32_8 40_8 64_11 80_15 104_8 128_15)) {
      #   my ($bits, $ebits) = split /_/, $args;
      #   next unless ($name ~~ $types->{'float'.$args});
      #   my $sbits = $bits - 1 - $ebits;  # remove sign bit and exponent bits = significand precision
      #
      #   # MAX = (2 - 2**(-$sbits-1)) * 2**($ebits-1)
      #   my $emax = $bigtwo->copy->bpow($ebits-1)->bsub(1);             # Y = (2**($ebits-1)-1)
      #   my $smin = $bigtwo->copy->bpow(-$sbits-1)->bmul(-1)->badd(2);  # Z = (2 - X) = -X + 2  (where X = 2**(-$sbits-1) )
      #   my $max  = $bigtwo->copy->bpow($emax)->bmul($smin);            # MAX = 2**Y * Z
      #
      #   # $max should have auto-upgraded (due to negative bpow), so create a new $bad version
      #   my $bad = $max;  # my bad?
      #   $bad =~ s/\..+$//g;  # can't give BigInt any decimals, either
      #   $bad = Math::BigInt->new($bad);
      #   $bad->upgrade('');
      #
      #   my $s = 0.0000000000001;
      #
      #   lives_ok { $obj->$sub( $max+0 ) } "$name: $args Float accepts  $max+0 (BigFloat)";
      #   lives_ok { $obj->$sub(-$max+0 ) } "$name: $args Float accepts -$max+0 (BigFloat)";
      #    dies_ok { $obj->$sub( $max+$s) } "$name: $args Float rejects  $max+$s (BigFloat)";
      #    dies_ok { $obj->$sub(-$max-$s) } "$name: $args Float rejects -$max-$s (BigFloat)";
      #    dies_ok { $obj->$sub($bad)     } "$name: $args Float rejects BigInt";
      #
      #   # this is a global, even when called by the OO *grumble*
      #   # see RT #78097: https://rt.cpan.org/Ticket/Display.html?id=78097
      #   Math::BigInt->upgrade('Math::BigFloat');
      #}
      #
      #foreach my $args (qw(32_7_96 64_16_384 128_34_6144)) {
      #   my ($bits, $digits, $emax) = split /_/, $args;
      #   next unless ($name eq 'Decimal'.$bits);
      #
      #   my $max = $bigten->copy->bpow($emax)->bsub(1);
      #
      #   my $bad = $max;  # my bad?
      #   $bad->upgrade('');
      #   $max = Math::BigFloat->new($max);
      #   my $s = 0.0000000000001;
      #
      #   lives_ok { $obj->$sub( $max+0 ) } "$name: Decimal$bits accepts  $max+0 (BigFloat)";
      #   lives_ok { $obj->$sub(-$max+0 ) } "$name: Decimal$bits accepts -$max+0 (BigFloat)";
      #    dies_ok { $obj->$sub( $max+$s) } "$name: Decimal$bits rejects  $max+$s (BigFloat)";
      #    dies_ok { $obj->$sub(-$max-$s) } "$name: Decimal$bits rejects -$max-$s (BigFloat)";
      #    dies_ok { $obj->$sub($bad)     } "$name: Decimal$bits rejects BigInt";
      #
      #   # this is a global, even when called by the OO *grumble*
      #   # see RT #78097: https://rt.cpan.org/Ticket/Display.html?id=78097
      #   Math::BigInt->upgrade('Math::BigFloat');
      #}
      #
      ## Char48/Char64 is going to accept every single character, because UTF-8 is 6 bytes.
      ## Ditto for Char32, since UTF-8 currently doesn't have anything beyond the U+1003FF codepage.
      #foreach my $bits (8,16,24,32) {
      #   # We can't just blindly make up FFFFFF characters; UTF-8 has a specific standard
      #   state $chars = {
      #       6 => chr 0x24,
      #       7 => chr 0x80,
      #       8 => chr 0xFF,
      #      16 => chr 0xC2A2,
      #      24 => chr 0xE282AC,
      #      32 => chr 0xF0A4ADA2,
      #   };
      #
      #   if ($name eq 'Char'.$bits) {
      #      foreach my $cb (sort { $a <=> $b } keys %$chars) {
      #         my $c = $chars->{$cb};
      #         ($bits >= $cb) ? lives_ok { $obj->$sub($c) } "$name: Char$bits accepts chr ".sprintf('%X', ord $c) :
      #                           dies_ok { $obj->$sub($c) } "$name: Char$bits rejects chr ".sprintf('%X', ord $c);
      #      }
      #   }
      #}
   };
}

done_testing;
