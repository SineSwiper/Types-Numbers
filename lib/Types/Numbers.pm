package Types::Numbers;

# VERSION
# ABSTRACT: Type constraints for numbers

#############################################################################
# Modules

use v5.8.8;
use strict;
use warnings;

# VERSION
# ABSTRACT: Parameterized C-like data types for Moo(se)

our @EXPORT_OK = ();

use Type::Library -base;
use Type::Tiny::Intersection;
use Type::Tiny::Union;
use Types::Standard ();

use Scalar::Util v1.20 (qw(blessed looks_like_number));  # support for overloaded/blessed looks_like_number
use POSIX qw(ceil);
use Math::BigFloat;
use Data::Float;
use Data::Integer;

use constant {
   _BASE2_LOG     => log(2) / log(10),
   _SAFE_NUM_MIN  => Data::Integer::min_signed_natint   < Data::Float::max_integer * -1 ?
                     Data::Integer::min_signed_natint   : Data::Float::max_integer * -1,
   _SAFE_NUM_MAX  => Data::Integer::max_unsigned_natint > Data::Float::max_integer *  1 ?
                     Data::Integer::max_unsigned_natint : Data::Float::max_integer *  1,
};

sub _croak ($;@) { require Type::Exception; goto \&Type::Exception::croak }

no warnings;  # don't warn on type checks

#############################################################################
# Basic globals

my $bigtwo = Math::BigFloat->new(2);
my $bigten = Math::BigFloat->new(10);

my $meta = __PACKAGE__->meta;

my $_true = sub { 1 };

#############################################################################
# Framework types

### XXX: No support for "$bits-bit" type error messages; consider using deep_explanation...

# Moose and Type::Tiny types both don't seem to support Math::Big* = Num.
# So, we have to start almost from stratch.
my $_NumLike = $meta->add_type(
   name       => 'NumLike',
   parent     => Types::Standard::Defined,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a number!" },
   constraint => sub { looks_like_number $_ },
   inlined    => sub { "Scalar::Util::looks_like_number($_[1])" },
);

my $_NumRange = $meta->add_type(
   name       => 'NumRange',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a number within the correct range!" },
   constraint => $_true,  # kinda pointless without the parameters
   inlined    => $_NumLike->inlined,
   constraint_generator => sub {
      my ($min, $max) = (shift, shift);
      looks_like_number($min) or _croak( "First parameter to NumRange[`n, `p] expected to be a number; got $min");
      looks_like_number($max) or _croak("Second parameter to NumRange[`n, `p] expected to be a number; got $max");

      return sub {
         my $val = $_;
         $val >= $min and $val <= $max;
      };
   },
   inline_generator => sub {
      my ($min, $max) = (shift, shift);
      $min = blessed($min)."\->new('$min')" if (blessed $min);
      $max = blessed($max)."\->new('$max')" if (blessed $max);

      return sub {
         my ($self, $val) = @_;
         use Devel::Dwarn;
         Dwarn $_[1];

         $self->parent->inline_check($val)." && $val >= $min and $val <= $max";
      };
   },
);

my $_NumRange_perlsafe_inline = $_NumRange->inline_generator->(_SAFE_NUM_MIN, _SAFE_NUM_MAX);

my $_IntLike = $meta->add_type(
   name       => 'IntLike',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not an integer!" },
   constraint => Types::Standard::Int->constraint,
   inlined    => Types::Standard::Int->inlined,
);

my $_BlessedNum = $meta->add_type( Type::Tiny::Intersection->new(
   name       => 'BlessedNum',
   library    => __PACKAGE__,
   message    => sub { "$_ is not a blessed number!" },
   type_constraints => [ $_NumLike, Types::Standard::Object ],
   constraint_generator => sub {
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedNum[`d] expected to be a positive integer; got $digits");

      return sub {
         my $val = $_;

         $val->can('accuracy')  and $val->accuracy  >= $digits or
         $val->can('precision') and $val->precision >= $digits or
         $val->can('div_scale') and $val->div_scale >= $digits;
      };
   },
   inline_generator => sub {
      my $digits = shift;

      return sub {
         my ($self, $val) = @_;

         '('.$self->inline_check($val).') and ('.
         '   '."$val\->can('accuracy')  and $val\->accuracy  >= $digits or".
         '   '."$val\->can('precision') and $val\->precision >= $digits or".
         '   '."$val\->can('div_scale') and $val\->div_scale >= $digits".
         ')';
      };
   },
) );

my $_NaN = $meta->add_type(
   name       => 'NaN',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a NaN!" },
   constraint => sub {
      my $val = $_;

      Types::Standard::Object->check($val) and $val->can('is_nan') and $val->is_nan or
      Data::Float::float_is_nan($val);
   },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val).' and ('.
      '   '.Types::Standard::Object->inline_check($val)." and $val\->can('is_nan') and $val\->is_nan or".
      '   '."Data::Float::float_is_nan($val)".
      ')';
   },
);

my $_Inf = $meta->add_type(
   name       => 'Inf',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not infinity!" },
   constraint => sub {
      my $val = $_;

      Types::Standard::Object->check($val) and $val->can('is_inf') and ($val->is_inf('+') or $val->is_inf('-')) or
      Data::Float::float_is_infinite($val);
   },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val).' and ('.
      '   '.Types::Standard::Object->inline_check($val)." and $val\->can('is_inf') and ($val\->is_inf('+') or $val\->is_inf('-')) or".
      '   '."Data::Float::float_is_infinite($val)".
      ')';
   },
   constraint_generator => sub {
      my $sign = shift;
      $sign =~ /\A[+\-]\z/ or _croak("Parameter to Inf[`s] expected to be a plus or minus sign; got $sign");

      return sub {
         my $val = $_;

         Types::Standard::Object->check($val) and $val->can('is_inf') and $val->is_inf($sign) or
         Data::Float::float_is_infinite($val) and Data::Float::float_sign($val) eq $sign;
      };
   },
   inline_generator => sub {
      my $sign = shift;

      return sub {
         my ($self, $val) = @_;

         $self->parent->inline_check($val).' and ('.
         '   '.Types::Standard::Object->inline_check($val)." and $val\->can('is_inf') and $val\->is_inf('$sign') or".
         '   '."Data::Float::float_is_infinite($val) and Data::Float::float_sign($val) eq '$sign'".
         ')';
      };
   },
);

my $_NaNInf = $_NaN | $_Inf;  # this is used a lot for floats

my $_RealNum = $meta->add_type( Type::Tiny::Intersection->new(
   name       => 'RealNum',
   library    => __PACKAGE__,
   message    => sub { "$_ is not a real number!" },
   type_constraints => [ $_NumLike, $_NaNInf->complementary_type ],
) );

#############################################################################
# Integer types

# Helper subs
sub __integer_bits_vars {
   my ($bits, $is_unsigned) = @_;

   my $sbits = $bits - 1;

   my ($neg, $spos, $upos) = (
      $bigtwo->copy->bpow($sbits)->bmul(-1),
      $bigtwo->copy->bpow($sbits)->bsub(1),
      $bigtwo->copy->bpow( $bits)->bsub(1),
   );
   my $sdigits = ceil( $sbits * _BASE2_LOG );
   my $udigits = ceil(  $bits * _BASE2_LOG );

   return $is_unsigned ?
      (0,    $upos, $udigits) :
      ($neg, $spos, $sdigits)
   ;
}

my $_PerlSafeInt = $meta->add_type(
   name       => 'PerlSafeInt',
   parent     => $_IntLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a Perl safe integer!" },
   constraint => sub { Types::Standard::Str->check($_) and $_ >= _SAFE_NUM_MIN and $_ <= _SAFE_NUM_MAX },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val).' and '.
      Types::Standard::Str->inline_check($val).' and '.
      $_NumRange_perlsafe_inline->($val);
   },
);

my $_BlessedInt = $meta->add_type( Type::Tiny::Intersection->new(
   name       => 'BlessedInt',
   library    => __PACKAGE__,
   message    => sub { "$_ is not a blessed integer!" },
   type_constraints => [ $_BlessedNum, $_IntLike ],
   constraint_generator => sub {
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedInt[`d] expected to be a positive integer; got $digits");

      my $_BlessedNum_check = $_BlessedNum->constraint_generator->($digits);

      return sub {
         $_IntLike->check($_) and $_BlessedNum_check->($_);
      };
   },
   inline_generator => sub {
      my $_BlessedNum_inline = $_BlessedNum->inline_generator->(shift);
      sub { $_IntLike->inline_check($_[1]).' && '.$_BlessedNum_inline->($_[1]); }
   },
) );

$meta->add_type(
   name       => 'SignedInt',
   parent     => $_IntLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a signed integer!" },
   constraint => sub { $_PerlSafeInt->check($_) or $_BlessedNum->check($_) },
   inlined    => sub {
      my $val = $_[1];
      $_IntLike->inline_check($val).' and ('.
         $_NumRange_perlsafe_inline->($val).' || '.$_BlessedNum->inline_check($val).
      ')';
   },
   constraint_generator => sub {
      my $bits = shift;
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to SignedInt[`b] expected to be a positive integer; got $bits");

      my ($min, $max, $digits) = __integer_bits_vars($bits, 0);
      my $_BlessedInt_check = $_BlessedInt->constraint_generator->($digits);
      my $_NumRange_check   = $_NumRange  ->constraint_generator->($min, $max);

      return sub {
         ($_PerlSafeInt->check($_) or $_BlessedInt_check->($_)) and
         $_NumRange_check->($_)
      };
   },
   inline_generator => sub {
      my $bits = shift;

      my ($min, $max, $digits) = __integer_bits_vars($bits, 0);
      my $_BlessedInt_inline = $_BlessedInt->inline_generator->($digits);
      my $_NumRange_inline   = $_NumRange  ->inline_generator->($min, $max);

      return sub {
         my $val = $_[1];
         '('.$_PerlSafeInt->inline_check($val).' || '.$_BlessedInt_inline->($val).') && '.
         $_NumRange_inline->($val);
      };
   },
);

$meta->add_type(
   name       => 'UnsignedInt',
   parent     => $_IntLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not an unsigned integer!" },
   constraint => sub { /\A[0-9]+\z/ and ($_PerlSafeInt->check($_) or $_BlessedNum->check($_)) },
   inlined    => sub {
      my $val = $_[1];
      "defined $val and $val =~ ".'/\A[0-9]+\z/ and ('.
         $_NumRange_perlsafe_inline->($val).' || '.
         $_BlessedNum->inline_check($val).
      ')';
   },
   constraint_generator => sub {
      my $bits = shift;
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to UnsignedInt[`b] expected to be a positive integer; got $bits");

      my ($min, $max, $digits) = __integer_bits_vars($bits, 1);
      my $_BlessedInt_check = $_BlessedInt->constraint_generator->($digits);
      my $_NumRange_check   = $_NumRange  ->constraint_generator->($min, $max);

      return sub {
         /\A[0-9]+\z/ and
         ($_PerlSafeInt->check($_) or $_BlessedInt_check->($_)) and
         $_NumRange_check->($_)
      };
   },
   inline_generator => sub {
      my $bits = shift;

      my ($min, $max, $digits) = __integer_bits_vars($bits, 1);
      my $_BlessedNum_inline = $_BlessedNum->inline_generator->($digits);
      my $_NumRange_inline   = $_NumRange  ->inline_generator->($min, $max);

      # inline will already have the RE check, and maybe not need the extra NumRange check
      my $perlsafe_inline = $min >= _SAFE_NUM_MIN and $max <= _SAFE_NUM_MAX ?
         sub { Types::Standard::Str->inline_check($_[0]) } :
         sub { Types::Standard::Str->inline_check($_[0]).' and '.$_NumRange_perlsafe_inline->($_[0]) }
      ;

      return sub {
         my $val = $_[1];
         "$val =~ ".'/\A[0-9]+\z/ and '.
         '('.$perlsafe_inline->($val).' || '.$_BlessedNum_inline->($val).') && '.
         $_NumRange_inline->($val);
      };
   },
);

#############################################################################
# Float/fixed types

my $_BlessedFloat = $meta->add_type(
   name       => 'BlessedFloat',
   parent     => $_BlessedNum,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a blessed floating-point number!" },
   constraint => sub { $_->can('bpi') and blessed($_)->bpi(3) == 3.14 },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." && $val\->can('bpi') and Scalar::Util::blessed($val)\->bpi(3) == 3.14";
   },
   constraint_generator => sub {
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedFloat[`d] expected to be a positive integer; got $digits");

      my $_BlessedNum_check = $_BlessedNum->constraint_generator->($digits);

      return sub {
         $_BlessedNum_check->($_) and $_->can('bpi') and blessed($_)->bpi(3) == 3.14;
      };
   },
   inline_generator => sub {
      my $_BlessedNum_inline = $_BlessedNum->inline_generator->(shift);
      sub {
         my ($self, $val) = @_;
         $_BlessedNum_inline->($val)." && $val\->can('bpi') and Scalar::Util::blessed($val)\->bpi(3) == 3.14"
      }
   },
);

sub __float_binary_bits_vars {
   my ($bits, $ebits) = @_;

   my $sbits = $bits - 1 - $ebits;  # remove sign bit and exponent bits = significand precision

   # MAX = (2 - 2**(-$sbits-1)) * 2**($ebits-1)
   my $emax = $bigtwo->copy->bpow($ebits-1)->bsub(1);             # Y = (2**($ebits-1)-1)
   my $smin = $bigtwo->copy->bpow(-$sbits-1)->bmul(-1)->badd(2);  # Z = (2 - X) = -X + 2  (where X = 2**(-$sbits-1) )
   my $max  = $bigtwo->copy->bpow($emax)->bmul($smin);            # MAX = 2**Y * Z

   my $is_perl_safe = (
      Data::Float::significand_bits >= $sbits &&
      Data::Float::max_finite_exp   >= 2 ** $ebits - 1 &&
      Data::Float::have_infinite &&
      Data::Float::have_nan
   );

   my $digits = ceil( $sbits * _BASE2_LOG );

   return ($is_perl_safe, $digits, -$max, $max);
}

sub __float_decimal_bits_vars {
   my ($digits, $emax) = @_;

   # We're not going to worry about the (extreme) edge case that
   # Perl might be compiled with decimal float NVs, but we still
   # need to convert to base-2.
   my $sbits = ceil( $digits / _BASE2_LOG );
   my $emax2 = ceil( $emax   / _BASE2_LOG );

   my $is_perl_safe = (
      Data::Float::significand_bits >= $sbits &&
      Data::Float::max_finite_exp   >= $emax2 &&
      Data::Float::have_infinite &&
      Data::Float::have_nan
   );

   my $max = $bigten->copy->bpow($emax)->bsub(1);

   return ($is_perl_safe, $digits, -$max, $max);
}

sub __fixed_binary_bits_vars {
   my ($bits, $scale) = @_;

   my $sbits = $bits - 1;

   # So, we have a base-10 scale and a base-2 set of $bits.  Lovely.
   # We can't actually figure out if it's Perl safe until we find the
   # $max, adjust with the $scale, and then go BACK to base-2 limits.
   my $div = $bigten->copy->bpow($scale);
   my ($neg, $pos) = (
      # bdiv returns (quo,rem) in list context :/
      scalar $bigtwo->copy->bpow($sbits)->bmul(-1)->bdiv($div),
      scalar $bigtwo->copy->bpow($sbits)->bsub(1)->bdiv($div),
   );

   my $digits = ceil( $sbits * _BASE2_LOG );
   my $emin2  = ceil( $scale / _BASE2_LOG );

   my $is_perl_safe = (
      Data::Float::significand_bits >= $sbits &&
      Data::Float::min_finite_exp   <= -$emin2
   );

   return ($is_perl_safe, $digits, $neg, $pos, 1);
}

sub __fixed_decimal_bits_vars {
   my ($digits, $scale) = @_;

   my $sdigits = $digits - $scale;  # significand digits

   my $sbits = ceil( $digits / _BASE2_LOG );
   my $emin2 = ceil( $scale  / _BASE2_LOG );

   my $is_perl_safe = (
      Data::Float::significand_bits >= $sbits &&
      Data::Float::min_finite_exp   <= -$emin2
   );

   my $max = $bigten->copy->bpow($sdigits)->bsub(1);

   return ($is_perl_safe, $digits, -$max, $max, 1);
}

### NOTE: These two are very close to another type, but there's just too many variables
### to throw into a typical type

sub __real_constraint_generator {
   my ($is_perl_safe, $digits, $neg, $pos, $no_naninf);
   my $_BlessedFloat_check = $_BlessedFloat->constraint_generator->($digits);
   my $_NumRange_check     = $_NumRange    ->constraint_generator->($neg, $pos);

   if ($no_naninf) {
      return $is_perl_safe ?
         $_NumRange_check :
         sub { $_BlessedFloat_check->($_) and $_NumRange_check->($_) }
      ;
   }
   else {
      return $is_perl_safe ?
         sub { $_NumRange_check->($_) or $_NaNInf->check($_) } :
         sub { $_BlessedFloat_check->($_) and ( $_NumRange_check->($_) or $_NaNInf->check($_) ); }
      ;
   }
}

sub __real_inline_generator {
   my ($is_perl_safe, $digits, $neg, $pos, $no_naninf);
   my $_BlessedFloat_inline = $_BlessedFloat->inline_generator->($digits);
   my $_NumRange_inline     = $_NumRange    ->inline_generator->($neg, $pos);

   if ($no_naninf) {
      return $is_perl_safe ?
         $_NumRange_inline :
         sub { $_BlessedFloat_inline->($_[1]).' && '.$_NumRange_inline->($_[1]) }
      ;
   }
   else {
      return $is_perl_safe ?
         sub { $_NumRange_inline->($_[1]).' || '.$_NaNInf->inline_check($_[1]) } :
         sub { $_BlessedFloat_inline->($_[1]).' && ('.$_NumRange_inline->($_[1]).' || '.$_NaNInf->inline_check($_[1]).')' }
      ;
   }
}

$meta->add_type(
   name       => 'FloatBinary',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a binary floating-point number within the correct range!" },
   constraint => $_true,  # kinda pointless without the parameters
   inlined    => $_NumLike->inlined,
   constraint_generator => sub {
      my ($bits, $ebits) = (shift, shift);
      $bits  =~ /\A[0-9]+\z/ or _croak( "First parameter to FloatBinary[`b, `e] expected to be a positive integer; got $bits");
      $ebits =~ /\A[0-9]+\z/ or _croak("Second parameter to FloatBinary[`b, `e] expected to be a positive integer; got $ebits");

      return __real_constraint_generator( __float_binary_bits_vars($bits, $ebits) );
   },
   inline_generator => sub {
      my ($bits, $ebits) = (shift, shift);
      return __real_inline_generator( __float_binary_bits_vars($bits, $ebits) );
   },
);

$meta->add_type(
   name       => 'FloatDecimal',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a decimal floating-point number within the correct range!" },
   constraint => $_true,  # kinda pointless without the parameters
   inlined    => $_NumLike->inlined,
   constraint_generator => sub {
      my ($digits, $emax) = (shift, shift);
      $digits =~ /\A[0-9]+\z/ or _croak( "First parameter to FloatDecimal[`d, `e] expected to be a positive integer; got $digits");
      $emax   =~ /\A[0-9]+\z/ or _croak("Second parameter to FloatDecimal[`d, `e] expected to be a positive integer; got $emax");

      return __real_constraint_generator( __float_decimal_bits_vars($digits, $emax) );
   },
   inline_generator => sub {
      my ($digits, $emax) = (shift, shift);
      return __real_inline_generator( __float_decimal_bits_vars($digits, $emax) );
   },
);

$meta->add_type(
   name       => 'FixedBinary',
   parent     => $_RealNum,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a binary fixed-point number within the correct range!" },
   constraint => $_true,  # kinda pointless without the parameters
   inlined    => $_NumLike->inlined,
   constraint_generator => sub {
      my ($bits, $scale) = (shift, shift);
      $bits  =~ /\A[0-9]+\z/ or _croak( "First parameter to FixedBinary[`b, `s] expected to be a positive integer; got $bits");
      $scale =~ /\A[0-9]+\z/ or _croak("Second parameter to FixedBinary[`b, `s] expected to be a positive integer; got $scale");

      return __real_constraint_generator( __fixed_binary_bits_vars($bits, $scale) );
   },
   inline_generator => sub {
      my ($bits, $scale) = (shift, shift);
      return __real_inline_generator( __fixed_binary_bits_vars($bits, $scale) );
   },
);

$meta->add_type(
   name       => 'FixedDecimal',
   parent     => $_RealNum,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a decimal fixed-point number within the correct range!" },
   constraint => $_true,  # kinda pointless without the parameters
   inlined    => $_NumLike->inlined,
   constraint_generator => sub {
      my ($digits, $scale) = (shift, shift);
      $digits =~ /\A[0-9]+\z/ or _croak( "First parameter to FixedDecimal[`d, `s] expected to be a positive integer; got $digits");
      $scale  =~ /\A[0-9]+\z/ or _croak("Second parameter to FixedDecimal[`d, `s] expected to be a positive integer; got $scale");

      return __real_constraint_generator( __fixed_decimal_bits_vars($digits, $scale) );
   },
   inline_generator => sub {
      my ($digits, $scale) = (shift, shift);
      return __real_inline_generator( __fixed_decimal_bits_vars($digits, $scale) );
   },
);

#############################################################################
# Character types

$meta->add_type(
   name       => 'Char',
   parent     => Types::Standard::Str,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a single character!" },
   constraint => sub { length($_) == 1 },  # length() will do a proper Unicode char length
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." and length($val) == 1";
   },
   constraint_generator => sub {
      my ($bits) = (shift);
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to Char[`b] expected to be a positive integer; got $bits");

      sub { length($_) == 1 and ord($_) < 2**$bits };
   },
   inline_generator => sub {
      my ($bits) = (shift);
      sub {
         my $val = $_[1];
         Types::Standard::Str->inline_check($val)." and length($val) == 1 and ord($val) < 2**$bits";
      }
   },
);

42;

### FINISH POD ###

#  Item (T:S)
#     Defined (T:S)
#        NumLike
#           NumRange
#           IntLike
#              PerlSafeInt
#              SignedInt[`b]
#              UnsignedInt[`b]
#           BlessedNum[`d]
#              BlessedInt[`d]
#              BlessedFloat[`d]
#           NaN
#           Inf
#           RealNum
#              FixedBinary[`b, `s]
#              FixedDecimal[`d, `s]
#           FloatBinary[`b, `e]
#           FloatDecimal[`d, `e]

#        Value (T:S)
#           Str (T:S)
#              Char[`b]

__END__

=head1 SYNOPSIS

   package MyPackage;
   use Moo;
   use MooX::Types::CLike qw(:all);

   has 'foo' => (
      isa => Int     # or Int32, Signed32
   );
   has 'bar' => (
      isa => Short   # or SmallInt, Int16, Signed16
   );

   use Scalar::Util qw(blessed);
   use Math::BigFloat;
   use Sub::Quote;

   has 'baz' => (
      isa => Double  # or Float64, Binary64

      # A Double number gets pretty big, so make sure we use big numbers
      coerce => quote_sub q{
         Math::BigFloat->new($_[0])
            unless (blessed $_[0] =~ /^Math::BigFloat|^bignum/);
      },
   );

=head1 DESCRIPTION

Given the popularity of various byte-sized data types in C-based languages, databases, and
computers in general, there's a need for validating those data types in Perl & Moo(se).  This
module covers the gamut of the various number and character types in all of those forms.

The number types will validate that the number falls within the right bit length, that unsigned
numbers do not go below zero, and "Perl unsafe" numbers are using either Math::Big* or
bignum/bigfloat.  (Whether a number is "Perl safe" depends on your Perl's
L<http://perldoc.perl.org/Config.html#ivsize|ivsize>, or data from L<Data::Float>.)  Big
numbers are also checked to make sure they have an accuracy that supports the right number of
significant decimal digits.  (However, BigInt/Float defaults to 40 digits, which is above the
34 digits for 128-bit numbers, so you should be safe.)

Char types will validate that it's a single character, using Perl's Unicode-complaint C<length>
function.  The bit check types (all of them other than C<WChar>) will also check that the
ASCII/Unicode code (C<ord>) is the right bit length.

IEEE 754 decimal floating types are also available, which are floats that use a base-10
mantissa.  And the SQL Server "money" types, which are basically decimal numbers stored as
integers.

=head1 TYPES


   Item (T:S)
      Defined (T:S)
         LaxNumLike
            FloatBinary[`b, `e]
            FloatDecimal[`d, `e]
            Money[`b, `s]
            NaN
            Inf
         StrictNumLike
            IntLike
               SignedInt[`b]
               UnsignedInt[`b]
         Value (T:S)
            Str (T:S)
               WChar
                  Char[`b]


All available types (including lots of aliases) are listed below:

   ### Integers ###
             [SIGNED]                                   | [UNSIGNED]
     4-bit = SNibble SSemiOctet Int4 Signed4            | Nibble SemiOctet UInt4 Unsigned4
     8-bit = SByte SOctet TinyInt Int8 Signed8          | Byte Octet UnsignedTinyInt UInt8 Unsigned8
    16-bit = Short SmallInt Int16 Signed16              | UShort UnsignedSmallInt UInt16 Unsigned16
    24-bit = MediumInt Int24 Signed24                   | UnsignedMediumInt UInt24 Unsigned24
    32-bit = Int Int32 Signed32                         | UInt UnsignedInt UInt32 Unsigned32
    64-bit = Long LongLong BigInt Int64 Signed64        | ULong ULongLong UnsignedBigInt UInt64 Unsigned64
   128-bit = SOctaWord SDoubleQuadWord Int128 Signed128 | OctaWord DoubleQuadWord UInt128 Unsigned128

   ### Floats (binary) ###
   (Total, Exponent) bits; Significand precision = Total - Exponent - 1 (sign bit)

   ( 16,  4) bits = ShortFloat
   ( 16,  5) bits = Half Float16 Binary16
   ( 32,  8) bits = Single Real Float Float32 Binary32
   ( 40,  8) bits = ExtendedSingle Float40
   ( 64, 11) bits = Double Float64 Binary64
   ( 80, 15) bits = ExtendedDouble Float80
   (104,  8) bits = Decimal    # not a IEEE 754 decimal, but C#'s bizarre "128-bit" float
   (128, 15) bits = Quadruple Quad Float128 Binary128

   ### Floats (decimal) ###
   (Digits, Exponent Max)

   ( 32,  8) = Decimal32
   ( 64, 11) = Decimal64
   (128, 15) = Decimal128

   ### "Money" ###
   (Bits, Scale)

   ( 32,  4) = SmallMoney
   ( 64,  4) = Money Currency
   (128,  6) = BigMoney  # doesn't exist; might change if it does suddenly exists

   ### Chars ###
   WChar = Single character (with Perl's natural Unicode-compliance)
   Bit check types = Char/Char8, Char16, Char32, Char48, Char64

=head1 EXPORTER TAGS

Since there are so many different aliases in this module, using C<:all> (while available) probably
isn't a good idea.  So, there are some Exporter tags available, grouped by language:

   # NOTE: Some extra types are included to fill in the gaps for signed vs. unsigned and
   # byte vs. char.

   :c       = Char Byte Short UShort Int UInt Long ULong Float Double ExtendedDouble
   :stdint  = Int4 UInt4 ... Int128 UInt128 (except 24-bit)
   :c#      = SByte Byte Char16 Short UShort Int UInt Long ULong Float Double Decimal
   :ieee754 = Binary16,32,64,128 and Decimal32,64,128
   :tsql    = TinyInt SmallInt Int BigInt SmallMoney Money Float64 Real
   :mysql   = TinyInt SmallInt MediumInt Int BigInt (and Unsigned versions) Float Double
   :ansisql = SmallInt Int Float Real Double

   :is_*    = All of the is_* functions for that tag
   :*+is    = Both the Moo and is_* functions for that tag

=head1 CAVEATS

The C<Int> type is also used by L<MooX::Types::MooseLike::Base>, and is even used (but not exported)
as a subtype for the Integer classes.  So be careful not to import both of them at the same time, as
they have different meanings.

Most C-based languages use a C<char> type to indicate both an 8-bit number and a single character,
as all strings in those languages are represented as a series of character codes.  Perl, as a dynamic
language, has a single scalar to represent both strings and numbers.  Thus, to separate the validation
of the two, the term C<Byte> or C<Octet> means the numeric 8-bit types, and the term C<Char> means the
single character string types.

The term C<long> in C/C++ is ambiguous depending on the bits of the OS: 32-bits for 32-bit OSs and
64-bits for 64-bit OSs.  Since the 64-bit version makes more sense (ie: C<short E<lt> int E<lt> long>),
that is the designation chosen.  To avoid confusion, you can just use C<LongLong> and C<ULongLong>.

The confusion is even worse for float types, with the C<long> modifier sometimes meaning absolutely
nothing in certain hardware platforms.  C<Long> isn't even used in this module for those types, in
favor of IEEE 754's "Extended" keyword.

The floats will support infinity and NaN, since C floats support this.  This may not be desirable, so
you might want to subtype the float and test for Inf/NaN if you don't want these.  Furthermore, the
"Perl safe" scalar tests for floats include checks to make sure it supports Inf/NaN.  However, the odds
of it NOT supporting those (since Perl should be using IEEE 754 floats for NV) are practically zero.

Hopefully, I've covered all possible types of floats found in the wild.  If not, let me know and I'll
add it in.  (For that matter, let me know if I'm missing I<any> type found in the wild.)

=cut
