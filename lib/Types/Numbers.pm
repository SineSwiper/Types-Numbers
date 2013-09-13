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
use Types::Standard ();

use Scalar::Util v1.20 (qw(blessed looks_like_number));  # support for overloaded/blessed looks_like_number
use POSIX 'ceil';
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

#############################################################################
# Framework types

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
   # kinda pointless without the parameters
   message    => $_NumLike->message,
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my ($min, $max) = (shift, shift);
      looks_like_number($min) or _croak( "First parameter to NumRange[`n, `p] expected to be a number; got $min");
      looks_like_number($max) or _croak("Second parameter to NumRange[`n, `p] expected to be a number; got $max");

      my ($Imin, $Imax) = ($min, $max);
      $Imin = blessed($min)."\->new('$min')" if (blessed $min);
      $Imax = blessed($max)."\->new('$max')" if (blessed $max);

      Type::Tiny->new(
         display_name => "NumRange[$min, $max]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a number between $min and $max!" },
         constraint => sub {
            my $val = $_;
            $val >= $min and $val <= $max;
         },
         inlined    => sub {
            my ($self, $val) = @_;
            $self->parent->inline_check($val)." && $val >= $Imin and $val <= $Imax";
         },
      );
   },
);

# we need to optimize out all of the NumLike checks
my $_NumRange_perlsafe = Type::Tiny->new(
   message    => sub { "$_ is not a Perl-safe number!" },
   # no equals because MAX+1 = MAX after truncation
   constraint => sub { $_ > _SAFE_NUM_MIN and $_ < _SAFE_NUM_MAX },
   inlined    => sub { $_[1].' > '._SAFE_NUM_MIN.' and '.$_[1].' < '._SAFE_NUM_MAX },
);

my $_IntLike = $meta->add_type(
   name       => 'IntLike',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { $_NumLike->validate($_) || "$_ is not an integer!" },
   constraint => sub { /\A-?[0-9]+\z/ },
   inlined    => sub { $_[1].' =~ /\A-?[0-9]+\z/' },
);

# This is basically LaxNum with a different parent
my $_PerlNum = $meta->add_type(
   name       => 'PerlNum',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { $_NumLike->validate($_) || "$_ is not a Perl number!" },
   constraint => Types::Standard::LaxNum->constraint,
   inlined    => Types::Standard::LaxNum->inlined,
);

my $_BlessedNum = $meta->add_type( Type::Tiny::Intersection->new(
   name         => 'BlessedNum',
   display_name => 'BlessedNum',
   library    => __PACKAGE__,
   message    => sub { $_NumLike->validate($_) || "$_ is not a blessed number!" },
   type_constraints => [ $_NumLike, Types::Standard::Object ],
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedNum[`d] expected to be a positive integer; got $digits");

      my $parent = $self;
      Type::Tiny->new(
         display_name => "BlessedNum[$digits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a blessed number capable of $digits digits!" },
         constraint => sub {
            my $val = $_;

            $val->can('accuracy')  and $val->accuracy  >= $digits or
            $val->can('div_scale') and $val->div_scale >= $digits;
         },
         inlined    => sub {
            my ($self, $val) = @_;

            $parent->inline_check($val).' and ( '.
               "$val->can('accuracy')  and $val->accuracy  >= $digits or ".
               "$val->can('div_scale') and $val->div_scale >= $digits ".
            ')';
         },
      );
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
      '   '.Types::Standard::Object->inline_check($val)." and $val->can('is_nan') and $val->is_nan or".
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
      '   '.Types::Standard::Object->inline_check($val)." and $val->can('is_inf') and ($val->is_inf('+') or $val->is_inf('-')) or".
      '   '."Data::Float::float_is_infinite($val)".
      ')';
   },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $sign = shift;
      $sign =~ /\A[+\-]\z/ or _croak("Parameter to Inf[`s] expected to be a plus or minus sign; got $sign");

      Type::Tiny->new(
         display_name => "Inf[$sign]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { "$_ is not ".($sign eq '+' ? 'positive' : 'negative')." infinity!" },
         constraint => sub {
            my $val = $_;

            Types::Standard::Object->check($val) and $val->can('is_inf') and $val->is_inf($sign) or
            Data::Float::float_is_infinite($val) and Data::Float::float_sign($val) eq $sign;
         },
         inlined    => sub {
            my ($self, $val) = @_;

            $self->parent->inline_check($val).' and ( '.
               Types::Standard::Object->inline_check($val)." and $val->can('is_inf') and $val->is_inf('$sign') or ".
               "Data::Float::float_is_infinite($val) and Data::Float::float_sign($val) eq '$sign' ".
            ')';
         },
      );
   },
);

# this is used a lot for floats, but we need to optimize out all of the NumLike checks
my $_NaNInf = Type::Tiny->new(
   name       => 'NaNInf',
   constraint => sub {
      # looks_like_number($_) and
      blessed($_) and (
         $_->can('is_nan') and $_->is_nan or
         $_->can('is_inf') and ($_->is_inf('+') or $_->is_inf('-'))
      ) or Data::Float::float_is_nan($_) or Data::Float::float_is_infinite($_)
   },
   inlined    => sub {
      my ($self, $val) = @_;
      # looks_like_number($val) and
      "Scalar::Util::blessed($val) and ( ".
         "$val->can('is_nan') and $val->is_nan or ".
         "$val->can('is_inf') and ($val->is_inf('+') or $val->is_inf('-')) ".
      ") or Data::Float::float_is_nan($val) or Data::Float::float_is_infinite($val)";
   },
);
my $_not_NaNInf = $_NaNInf->complementary_type;

my $_RealNum = $meta->add_type( Type::Tiny::Intersection->new(
   name       => 'RealNum',
   display_name => 'RealNum',
   library    => __PACKAGE__,
   message    => sub { $_NumLike->validate($_) || "$_ is not a real number!" },
   type_constraints => [ $_NumLike, $_not_NaNInf ],
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
   parent     => $_PerlNum,
   library    => __PACKAGE__,
   message    => sub { $_PerlNum->validate($_) || $_IntLike->validate($_) || $_NumRange_perlsafe->validate($_) },
   constraint => sub { /\A-?[0-9]+\z/ and $_NumRange_perlsafe->check($_) },
   inlined    => sub { "!ref($_[1]) and $_[1] =~ ".'/\\A-?[0-9]+\\z/ and '.$_NumRange_perlsafe->inline_check($_[1]) },
);

my $_BlessedInt = $meta->add_type(
   name       => 'BlessedInt',
   parent     => $_BlessedNum,
   library    => __PACKAGE__,
   message    => sub { $_BlessedNum->validate($_) || $_IntLike->validate($_) },
   constraint => sub { $_IntLike->check($_) },
   inlined    => sub { Types::Standard::Object->inline_check($_[1]).' and '.$_[1].' =~ /\\A-?[0-9]+\\z/' },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedInt[`d] expected to be a positive integer; got $digits");

      my $_BlessedNum_param = $_BlessedNum->parameterize($digits);

      Type::Tiny->new(
         display_name => "BlessedInt[$digits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a blessed integer capable of $digits digits!" },
         constraint => sub { $_IntLike->check($_) and $_BlessedNum_param->check($_) },
         inlined    => sub { $_IntLike->inline_check($_[1]).' && '.$_BlessedNum_param->inline_check($_[1]) },
      );
   },
);

$meta->add_type(
   name       => 'SignedInt',
   parent     => $_IntLike,
   library    => __PACKAGE__,
   message    => sub { $_IntLike->validate($_) || "$_ is not a signed integer!" },
   constraint => sub { $_PerlSafeInt->check($_) or $_BlessedNum->check($_) },
   inlined    => sub {
      my $val = $_[1];
      $_IntLike->inline_check($val).' and ('.
         $_NumRange_perlsafe->inline_check($val).' || '.Types::Standard::Object->inline_check($val).
      ')';
   },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $bits = shift;
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to SignedInt[`b] expected to be a positive integer; got $bits");

      my ($min, $max, $digits) = __integer_bits_vars($bits, 0);
      my $_BlessedInt_param = $_BlessedInt->parameterize($digits);
      my $_NumRange_param   = $_NumRange  ->parameterize($min, $max);

      Type::Tiny->new(
         display_name => "SignedInt[$bits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a $bits-bit signed integer!" },
         constraint => sub {
            ($_PerlSafeInt->check($_) or $_BlessedInt_param->check($_)) and
            $_NumRange_param->check($_)
         },
         inlined    => sub {
            my $val = $_[1];
            '('.$_PerlSafeInt->inline_check($val).' || '.$_BlessedInt_param->inline_check($val).') && '.
            $_NumRange_param->inline_check($val);
         },
      );
   },
);

$meta->add_type(
   name       => 'UnsignedInt',
   parent     => $_IntLike,
   library    => __PACKAGE__,
   message    => sub { $_IntLike->validate($_) || "$_ is not an unsigned integer!" },
   constraint => sub { /\A[0-9]+\z/ and ($_PerlSafeInt->check($_) or $_BlessedNum->check($_)) },
   inlined    => sub {
      my $val = $_[1];
      "$val =~ ".'/\A[0-9]+\z/ and ('.
         $_NumRange_perlsafe->inline_check($val).' || '.Types::Standard::Object->inline_check($val).
      ')';
   },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $bits = shift;
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to UnsignedInt[`b] expected to be a positive integer; got $bits");

      my ($min, $max, $digits) = __integer_bits_vars($bits, 1);
      my $_BlessedNum_param = $_BlessedNum->parameterize($digits);  # Int check already in RE
      my $_NumRange_param   = $_NumRange  ->parameterize($min, $max);

      # inline will already have the RE check, and maybe not need the extra NumRange check
      my $perlsafe_inline = $min >= _SAFE_NUM_MIN && $max <= _SAFE_NUM_MAX ?
         sub { Types::Standard::Str->inline_check($_[0]) } :
         sub { '('.Types::Standard::Str->inline_check($_[0]).' and '.$_NumRange_perlsafe->inline_check($_[0]).')' }
      ;

      Type::Tiny->new(
         display_name => "UnsignedInt[$bits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a $bits-bit unsigned integer!" },
         constraint => sub {
            /\A[0-9]+\z/ and
            ($_PerlSafeInt->check($_) or $_BlessedNum_param->check($_)) and
            $_NumRange_param->check($_)
         },
         inlined    => sub {
            my $val = $_[1];
            "$val =~ ".'/\A[0-9]+\z/ and '.
            '('.$perlsafe_inline->($val).' || '.$_BlessedNum_param->inline_check($val).') && '.
            $_NumRange_param->inline_check($val);
         },
      );
   },
);

#############################################################################
# Float/fixed types

my $_BlessedFloat = $meta->add_type(
   name       => 'BlessedFloat',
   parent     => $_BlessedNum,
   library    => __PACKAGE__,
   message    => sub { $_BlessedNum->validate($_) || "$_ is not a blessed floating-point number!" },
   constraint => sub { $_->can('bpi') and blessed($_)->bpi(3) == 3.14 },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." && $val->can('bpi') and Scalar::Util::blessed($val)\->bpi(3) == 3.14";
   },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedFloat[`d] expected to be a positive integer; got $digits");

      my $_BlessedNum_param = $_BlessedNum->parameterize($digits);

      Type::Tiny->new(
         display_name => "BlessedFloat[$digits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a blessed floating-point number capable of $digits digits!" },
         constraint => sub { $_BlessedNum_param->check($_) and $_->can('bpi') and blessed($_)->bpi(3) == 3.14 },
         inlined    => sub {
            my ($self, $val) = @_;
            $_BlessedNum_param->inline_check($val)." && $val->can('bpi') and Scalar::Util::blessed($val)\->bpi(3) == 3.14";
         },
      );
   },
);

my $_FloatSafeNum = $meta->add_type(
   name       => 'FloatSafeNum',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   message    => sub { $_NumLike->validate($_) || "$_ is not a floating-point-safe number!" },
   constraint => sub { $_PerlNum->check($_) or $_BlessedFloat->check($_) },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val).' and ('.
         "!ref($val) and (".
            $_NumRange_perlsafe->inline_check($val)." or Data::Float::float_is_nan($val) or Data::Float::float_is_infinite($val)".
         ') or '.
         "Scalar::Util::blessed($val) and $val->can('bpi') and Scalar::Util::blessed($val)->bpi(3) == 3.14".
      ')';
   },
);

my $_RealSafeNum = $meta->add_type(
   name       => 'RealSafeNum',
   parent     => $_RealNum,
   library    => __PACKAGE__,
   message    => sub { $_NumLike->validate($_) || $_RealNum->validate($_) || "$_ is not a real-safe number!" },
   constraint => sub { ( $_PerlNum->check($_) or $_BlessedFloat->check($_) ) and $_not_NaNInf->check($_) },
   inlined    => sub {
      my ($self, $val) = @_;
      $_NumLike->inline_check($val).' and ('.
         "( !ref($val) and ".$_NumRange_perlsafe->inline_check($val)." and not (".
            "Data::Float::float_is_nan($val) or Data::Float::float_is_infinite($val))".
         ') or ('.
            "Scalar::Util::blessed($val) and $val->can('bpi') and Scalar::Util::blessed($val)->bpi(3) == 3.14 and ".
            "not ($val->can('is_nan') and $val->is_nan or $val->can('is_inf') and ($val->is_inf('+') or $val->is_inf('-')) )".
         ')'.
      ')';
   },
);

### NOTE: These two are very close to another type, but there's just too many variables
### to throw into a typical type

### FIXME: 03-float.t is dreadfully slow!

sub __real_constraint_generator {
   my ($is_perl_safe, $digits, $_NumRange_param, $no_naninf) = @_;
   my $_BlessedFloat_param = $_BlessedFloat->parameterize($digits);

   if ($no_naninf) {
      return $is_perl_safe ?
         sub { ( $_PerlNum->check($_) or $_BlessedFloat_param->check($_) ) and $_NumRange_param->check($_) } :
         sub { $_BlessedFloat_param->check($_) and $_NumRange_param->check($_) }
      ;
   }
   else {
      return $is_perl_safe ?
         sub { ( $_PerlNum->check($_) or $_BlessedFloat_param->check($_) ) and $_NumRange_param->check($_) or $_NaNInf->check($_) } :
         sub { $_BlessedFloat_param->check($_) and ( $_NumRange_param->check($_) or $_NaNInf->check($_) ); }
      ;
   }
}

sub __real_inline_generator {
   my ($is_perl_safe, $digits, $_NumRange_param, $no_naninf) = @_;
   my $_BlessedFloat_param = $_BlessedFloat->parameterize($digits);

   if ($no_naninf) {
      return $is_perl_safe ?
         sub {
            '( '.$_PerlNum->inline_check($_[1]).' || '.$_BlessedFloat_param->inline_check($_[1]).' )'.
            ' && '.$_NumRange_param->inline_check($_[1])
         } :
         sub { $_BlessedFloat_param->inline_check($_[1]).' && '.$_NumRange_param->inline_check($_[1]) }
      ;
   }
   else {
      return $is_perl_safe ?
         sub {
            '( '.$_PerlNum->inline_check($_[1]).' || '.$_BlessedFloat_param->inline_check($_[1]).' )'.
            ' && ( '.$_NumRange_param->inline_check($_[1]).' || '.$_NaNInf->inline_check($_[1]).' )'
         } :
         sub { $_BlessedFloat_param->inline_check($_[1]).' && ('.$_NumRange_param->inline_check($_[1]).' || '.$_NaNInf->inline_check($_[1]).')' }
      ;
   }
}

$meta->add_type(
   name       => 'FloatBinary',
   parent     => $_FloatSafeNum,
   library    => __PACKAGE__,
   # kinda pointless without the parameters
   message    => $_FloatSafeNum->message,
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my ($bits, $ebits) = (shift, shift);
      $bits  =~ /\A[0-9]+\z/ or _croak( "First parameter to FloatBinary[`b, `e] expected to be a positive integer; got $bits");
      $ebits =~ /\A[0-9]+\z/ or _croak("Second parameter to FloatBinary[`b, `e] expected to be a positive integer; got $ebits");

      my $sbits = $bits - 1 - $ebits;  # remove sign bit and exponent bits = significand precision

      # MAX = (2 - 2**(-$sbits-1)) * 2**($ebits-1)
      my $emax = $bigtwo->copy->bpow($ebits-1)->bsub(1);             # Y = (2**($ebits-1)-1)
      my $smin = $bigtwo->copy->bpow(-$sbits-1)->bmul(-1)->badd(2);  # Z = (2 - X) = -X + 2  (where X = 2**(-$sbits-1) )
      my $max  = $bigtwo->copy->bpow($emax)->bmul($smin);            # MAX = 2**Y * Z

      my $digits = ceil( $sbits * _BASE2_LOG );

      my $is_perl_safe = (
         Data::Float::significand_bits >= $sbits &&
         Data::Float::max_finite_exp   >= 2 ** $ebits - 1 &&
         Data::Float::have_infinite &&
         Data::Float::have_nan
      );

      my $_NumRange_param = $_NumRange->parameterize(-$max, $max);

      Type::Tiny->new(
         display_name => "FloatBinary[$bits, $ebits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || $_NumRange_param->validate($_) || "$_ is not a $bits-bit floating-point number!" },
         constraint => __real_constraint_generator($is_perl_safe, $digits, $_NumRange_param),
         inlined    => __real_inline_generator    ($is_perl_safe, $digits, $_NumRange_param),
      );
   },
);

$meta->add_type(
   name       => 'FloatDecimal',
   parent     => $_FloatSafeNum,
   library    => __PACKAGE__,
   # kinda pointless without the parameters
   message    => $_FloatSafeNum->message,
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my ($digits, $emax) = (shift, shift);
      $digits =~ /\A[0-9]+\z/ or _croak( "First parameter to FloatDecimal[`d, `e] expected to be a positive integer; got $digits");
      $emax   =~ /\A[0-9]+\z/ or _croak("Second parameter to FloatDecimal[`d, `e] expected to be a positive integer; got $emax");

      # We're not going to worry about the (extreme) edge case that
      # Perl might be compiled with decimal float NVs, but we still
      # need to convert to base-2.
      my $sbits = ceil( $digits / _BASE2_LOG );
      my $emax2 = ceil( $emax   / _BASE2_LOG );

      my $max = $bigten->copy->bpow($emax)->bmul( '9.'.('9' x ($digits-1)) );

      my $is_perl_safe = (
         Data::Float::significand_bits >= $sbits &&
         Data::Float::max_finite_exp   >= $emax2 &&
         Data::Float::have_infinite &&
         Data::Float::have_nan
      );

      my $_NumRange_param = $_NumRange->parameterize(-$max, $max);

      Type::Tiny->new(
         display_name => "FloatDecimal[$digits, $emax]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || $_NumRange_param->validate($_) || "$_ is not a $digits-digit (significand) floating-point number!" },
         constraint => __real_constraint_generator($is_perl_safe, $digits, $_NumRange_param),
         inlined    => __real_inline_generator    ($is_perl_safe, $digits, $_NumRange_param),
      );
   },
);

$meta->add_type(
   name       => 'FixedBinary',
   parent     => $_RealSafeNum,
   library    => __PACKAGE__,
   # kinda pointless without the parameters
   message    => $_RealSafeNum->message,
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my ($bits, $scale) = (shift, shift);
      $bits  =~ /\A[0-9]+\z/ or _croak( "First parameter to FixedBinary[`b, `s] expected to be a positive integer; got $bits");
      $scale =~ /\A[0-9]+\z/ or _croak("Second parameter to FixedBinary[`b, `s] expected to be a positive integer; got $scale");

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

      my $_NumRange_param = $_NumRange->parameterize($neg, $pos);

      Type::Tiny->new(
         display_name => "FixedBinary[$bits, $scale]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || $_NumRange_param->validate($_) || "$_ is not a $bits-bit fixed-point number!" },
         constraint => __real_constraint_generator($is_perl_safe, $digits, $_NumRange_param, 1),
         inlined    => __real_inline_generator    ($is_perl_safe, $digits, $_NumRange_param, 1),
      );
   },
);

$meta->add_type(
   name       => 'FixedDecimal',
   parent     => $_RealSafeNum,
   library    => __PACKAGE__,
   message    => sub { "$_ is not a decimal fixed-point number within the correct range!" },
   # kinda pointless without the parameters
   message    => $_RealSafeNum->message,
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my ($digits, $scale) = (shift, shift);
      $digits =~ /\A[0-9]+\z/ or _croak( "First parameter to FixedDecimal[`d, `s] expected to be a positive integer; got $digits");
      $scale  =~ /\A[0-9]+\z/ or _croak("Second parameter to FixedDecimal[`d, `s] expected to be a positive integer; got $scale");

      my $sbits = ceil( $digits / _BASE2_LOG );
      my $emin2 = ceil( $scale  / _BASE2_LOG );

      my $is_perl_safe = (
         Data::Float::significand_bits >= $sbits &&
         Data::Float::min_finite_exp   <= -$emin2
      );

      my $div = $bigten->copy->bpow($scale);
      my $max = $bigten->copy->bpow($digits)->bsub(1)->bdiv($div);

      my $_NumRange_param = $_NumRange->parameterize(-$max, $max);

      Type::Tiny->new(
         display_name => "FixedDecimal[$digits, $scale]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || $_NumRange_param->validate($_) || "$_ is not a $digits-digit (significand) fixed-point number!" },
         constraint => __real_constraint_generator($is_perl_safe, $digits, $_NumRange_param, 1),
         inlined    => __real_inline_generator    ($is_perl_safe, $digits, $_NumRange_param, 1),
      );
   },
);

#############################################################################
# Character types

$meta->add_type(
   name       => 'Char',
   parent     => Types::Standard::Str,
   library    => __PACKAGE__,
   message    => sub { Types::Standard::Str->validate($_) || "$_ is not a single character!" },
   constraint => sub { length($_) == 1 },  # length() will do a proper Unicode char length
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." and length($val) == 1";
   },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my ($bits) = (shift);
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to Char[`b] expected to be a positive integer; got $bits");

      Type::Tiny->new(
         display_name => "Char[$bits]",
         parent     => $self,
         library    => __PACKAGE__,
         message    => sub { $self->validate($_) || "$_ is not a $bits-bit character!" },
         constraint => sub { length($_) == 1 and ord($_) < 2**$bits },
         inlined    => sub {
            my $val = $_[1];
            Types::Standard::Str->inline_check($val)." and length($val) == 1 and ord($val) < 2**$bits";
         },
      );
   },
);

42;

### FINISH POD ###

#  Item (T:S)
#     Defined (T:S)
#        NumLike
#           NumRange
#           IntLike
#              SignedInt[`b]
#              UnsignedInt[`b]
#           PerlNum
#              PerlSafeInt
#           BlessedNum[`d]
#              BlessedInt[`d]
#              BlessedFloat[`d]
#           NaN
#           Inf
#           FloatSafeNum
#              FloatBinary[`b, `e]
#              FloatDecimal[`d, `e]
#           RealNum
#              RealSafeNum
#                 FixedBinary[`b, `s]
#                 FixedDecimal[`d, `s]

#        Value (T:S)
#           Str (T:S)
#              Char[`b]


42;

__END__

=begin wikidoc

= DESCRIPTION

### Ruler ########################################################################################################################12345

Insert description here...

= CAVEATS

### Ruler ########################################################################################################################12345

Bad stuff...

= SEE ALSO

### Ruler ########################################################################################################################12345

= ACKNOWLEDGEMENTS

### Ruler ########################################################################################################################12345

Thanks and stuff...

=end wikidoc
