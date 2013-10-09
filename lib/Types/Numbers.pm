package Types::Numbers;

our $VERSION = '0.91_01'; # VERSION
# ABSTRACT: Type constraints for numbers

#############################################################################
# Modules

use v5.8.8;
use strict;
use warnings;

our @EXPORT_OK = ();

use Type::Library -base;
use Type::Tiny::Intersection;
use Type::Tiny::Union;
use Types::Standard ();

use Scalar::Util v1.20 (qw(blessed looks_like_number));  # support for overloaded/blessed looks_like_number
use POSIX 'ceil';
use Math::BigInt;
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

### TODO: Coercions where safe ###

# Moose and Type::Tiny types both don't seem to support Math::Big* = Num.
# So, we have to start almost from stratch.
my $_NumLike = $meta->add_type(
   name       => 'NumLike',
   parent     => Types::Standard::Defined,
   library    => __PACKAGE__,
   constraint => sub { looks_like_number $_ },
   inlined    => sub { "Scalar::Util::looks_like_number($_[1])" },
);

my $_NumRange = $meta->add_type(
   name       => 'NumRange',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   # kinda pointless without the parameters
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

# Large 64-bit integers tend to stringify themselves in exponent notation,
# even though the number is still pristine.  IOW, the numeric form is perfect,
# but the string form loses information.  This can be a problem for stringified
# inlines.
my $SAFE_NUM_MIN_STR = Math::BigInt->new( _SAFE_NUM_MIN )->bstr;
my $SAFE_NUM_MAX_STR = Math::BigInt->new( _SAFE_NUM_MAX )->bstr;

# we need to optimize out all of the NumLike checks
my $_NumRange_perlsafe = Type::Tiny->new(
   display_name => "_NumRange_perlsafe",
   parent     => $_NumLike,
   # no equals because MAX+1 = MAX after truncation
   constraint => sub { $_ > _SAFE_NUM_MIN and $_ < _SAFE_NUM_MAX },
   inlined    => sub { "$_[1] > ".$SAFE_NUM_MIN_STR." and $_[1] < ".$SAFE_NUM_MAX_STR },
);

### XXX: This string equality check is necessary because Math::BigInt seems to think 1.5 == 1
my $_IntLike = $meta->add_type(
   name       => 'IntLike',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   constraint => sub { /\d+/ and int($_) == $_ and int($_) eq $_ },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." and $val =~ /\\d+/ and int($val) == $val and int($val) eq $val";
   },
);

# This is basically LaxNum with a different parent
my $_PerlNum = $meta->add_type(
   name       => 'PerlNum',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   constraint => Types::Standard::LaxNum->constraint,
   inlined    => Types::Standard::LaxNum->inlined,
);

my $_BlessedNum = $meta->add_type( Type::Tiny::Intersection->new(
   name         => 'BlessedNum',
   display_name => 'BlessedNum',
   library    => __PACKAGE__,
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
my $_NaNInf = Type::Tiny::Union->new(
   type_constraints => [ $_NaN, $_Inf ],
)->create_child_type(
   name       => 'NaNInf',
   constraint => sub {
      # looks_like_number($_) and
      Types::Standard::Object->check($_) and (
         $_->can('is_nan') and $_->is_nan or
         $_->can('is_inf') and ($_->is_inf('+') or $_->is_inf('-'))
      ) or Data::Float::float_is_nan($_) or Data::Float::float_is_infinite($_)
   },
   inlined    => sub {
      my ($self, $val) = @_;
      # looks_like_number($val) and
      Types::Standard::Object->inline_check($val)." and ( ".
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

my $_PerlSafeInt = $meta->add_type( Type::Tiny::Intersection->new(
   library    => __PACKAGE__,
   type_constraints => [ $_PerlNum, $_IntLike, $_NumRange_perlsafe ],
)->create_child_type(
   name       => 'PerlSafeInt',
   library    => __PACKAGE__,
   inlined    => sub { "!ref($_[1]) and ".$_IntLike->inline_check($_[1]).' and '.$_NumRange_perlsafe->inline_check($_[1]) },
) );

my $_BlessedInt = $meta->add_type( Type::Tiny::Intersection->new(
   library    => __PACKAGE__,
   type_constraints => [ $_BlessedNum, $_IntLike ],
)->create_child_type(
   name       => 'BlessedInt',
   library    => __PACKAGE__,
   inlined    => sub { Types::Standard::Object->inline_check($_[1]).' and '.$_IntLike->inline_check($_[1]) },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $digits = shift;
      $digits =~ /\A[0-9]+\z/ or _croak("Parameter to BlessedInt[`d] expected to be a positive integer; got $digits");

      my $_BlessedNum_param = $_BlessedNum->parameterize($digits);

      Type::Tiny->new(
         display_name => "BlessedInt[$digits]",
         parent     => $self,
         library    => __PACKAGE__,
         constraint => sub {
            $_IntLike->check($_) && $_BlessedNum_param->check($_) && do {
               my $num = $_;
               $num =~ s/\D+//g;
               length($num) <= $digits
            }
         },
         inlined    => sub {
            $_IntLike->inline_check($_[1]).' && '.$_BlessedNum_param->inline_check($_[1]).' && do { '.
               'my $num = '.$_[1].'; '.
               '$num =~ s/\D+//g; '.
               'length($num) <= '.$digits.' '.
            '}';
         },
      );
   },
) );

$meta->add_type( Type::Tiny::Union->new(
   #parent     => $_IntLike,
   library    => __PACKAGE__,
   type_constraints => [ $_PerlSafeInt, $_BlessedInt ],
)->create_child_type(
   name       => 'SignedInt',
   library    => __PACKAGE__,
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

      Type::Tiny::Intersection->new(
         library    => __PACKAGE__,
         type_constraints => [ $self, ($_PerlSafeInt|$_BlessedInt_param), $_NumRange_param ],
      )->create_child_type(
         display_name => "SignedInt[$bits]",
         inlined    => sub {
            my $val = $_[1];
            '('.$_PerlSafeInt->inline_check($val).' || '.$_BlessedInt_param->inline_check($val).') && '.
            $_NumRange_param->inline_check($val);
         },
      );
   },
) );

$meta->add_type(
   name       => 'UnsignedInt',
   parent     => $_IntLike,
   library    => __PACKAGE__,
   constraint => sub { $_IntLike->check($_) and $_ >= 0 and ($_PerlSafeInt->check($_) or $_BlessedNum->check($_)) },
   inlined    => sub {
      my $val = $_[1];
      $_IntLike->inline_check($val)." and $val >= 0 and (".
         $_NumRange_perlsafe->inline_check($val).' || '.Types::Standard::Object->inline_check($val).
      ')';
   },
   constraint_generator => sub {
      my $self = $Type::Tiny::parameterize_type;
      my $bits = shift;
      $bits =~ /\A[0-9]+\z/ or _croak("Parameter to UnsignedInt[`b] expected to be a positive integer; got $bits");

      my ($min, $max, $digits) = __integer_bits_vars($bits, 1);
      my $_BlessedNum_param = $_BlessedNum->parameterize($digits);  # IntLike check extracted out
      my $_NumRange_param   = $_NumRange  ->parameterize($min, $max);

      # inline will already have the IntLike check, and maybe not need the extra NumRange check
      my $perlsafe_inline = $min >= _SAFE_NUM_MIN && $max <= _SAFE_NUM_MAX ?
         sub { Types::Standard::Str->inline_check($_[0]) } :
         sub { '('.Types::Standard::Str->inline_check($_[0]).' and '.$_NumRange_perlsafe->inline_check($_[0]).')' }
      ;

      Type::Tiny->new(
         display_name => "UnsignedInt[$bits]",
         parent     => $self,
         library    => __PACKAGE__,
         constraint => sub {
            $_IntLike->check($_) and $_NumRange_param->check($_) and
            ($_PerlSafeInt->check($_) or $_BlessedNum_param->check($_));
         },
         inlined    => sub {
            my $val = $_[1];
            $_IntLike->inline_check($val).' and '.$_NumRange_param->inline_check($val).' and '.
            '('.$perlsafe_inline->($val).' or '.$_BlessedNum_param->inline_check($val).')';
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
   constraint => sub { blessed($_)->new(1.2) == 1.2 },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." && Scalar::Util::blessed($val)\->new(1.2) == 1.2";
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
         constraint => sub { $_BlessedNum_param->check($_) and blessed($_)->new(1.2) == 1.2 },
         inlined    => sub {
            my ($self, $val) = @_;
            $_BlessedNum_param->inline_check($val)." and Scalar::Util::blessed($val)\->new(1.2) == 1.2";
         },
      );
   },
);

my $_PerlSafeFloat = $meta->add_type(
   name       => 'PerlSafeFloat',
   parent     => $_PerlNum,
   library    => __PACKAGE__,
   constraint => sub { $_NumRange_perlsafe->check($_) or Data::Float::float_is_nan($_) or Data::Float::float_is_infinite($_) },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val).' and ('.
         $_NumRange_perlsafe->inline_check($val)." or Data::Float::float_is_nan($val) or Data::Float::float_is_infinite($val)".
      ')';
   },
);

my $_FloatSafeNum = $meta->add_type( Type::Tiny::Union->new(
   library    => __PACKAGE__,
   type_constraints => [ $_PerlSafeFloat, $_BlessedFloat ],
)->create_child_type(
   name       => 'FloatSafeNum',
   library    => __PACKAGE__,
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val).' and ('.
         "!ref($val) and (".
            $_NumRange_perlsafe->inline_check($val)." or Data::Float::float_is_nan($val) or Data::Float::float_is_infinite($val)".
         ') or '.
         Types::Standard::Object->inline_check($val)." and Scalar::Util::blessed($val)->new(1.2) == 1.2".
      ')';
   },
) );

my $_RealSafeNum = $meta->add_type( Type::Tiny::Intersection->new(
   library    => __PACKAGE__,
   type_constraints => [ $_RealNum, $_FloatSafeNum ],
)->create_child_type(
   name       => 'RealSafeNum',
   library    => __PACKAGE__,
   inlined    => sub {
      my ($self, $val) = @_;
      $_NumLike->inline_check($val).' and ('.
         "( !ref($val) and ".$_NumRange_perlsafe->inline_check($val)." and not (".
            "Data::Float::float_is_nan($val) or Data::Float::float_is_infinite($val))".
         ') or ('.
            Types::Standard::Object->inline_check($val)." and Scalar::Util::blessed($val)->new(1.2) == 1.2 and ".
            "not ($val->can('is_nan') and $val->is_nan or $val->can('is_inf') and ($val->is_inf('+') or $val->is_inf('-')) )".
         ')'.
      ')';
   },
) );

### NOTE: These two are very close to another type, but there's just too many variables
### to throw into a typical type

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
         constraint => __real_constraint_generator($is_perl_safe, $digits, $_NumRange_param, 1),
         inlined    => __real_inline_generator    ($is_perl_safe, $digits, $_NumRange_param, 1),
      );
   },
);

$meta->add_type(
   name       => 'FixedDecimal',
   parent     => $_RealSafeNum,
   library    => __PACKAGE__,
   # kinda pointless without the parameters
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
         constraint => sub { ord($_) < 2**$bits },
         inlined    => sub {
            my $val = $_[1];
            Types::Standard::Str->inline_check($val)." and length($val) == 1 and ord($val) < 2**$bits";
         },
      );
   },
);

42;

__END__

=pod

=encoding utf-8

=head1 NAME

Types::Numbers - Type constraints for numbers

=head1 DESCRIPTION

Because we deal with numbers every day in our programs and modules, this is an extensive L<Type::Tiny> library of number validations.
Like L<Type::Tiny>, these types work with all modern OO platforms and as a standalone type system.

=encoding utf8

=head1 TYPES

=head2 Overview

All of these types strive for the accurate storage and validation of many different types of numbers, including some storage types
that Perl doesn't natively support.

The hierarchy of the types is as follows:

    (T:S = From Types::Standard)
 
    Item (T:S)
       Defined (T:S)
          NumLike
             NumRange[`n, `p]
             IntLike
                SignedInt[`b]
                UnsignedInt[`b]
             PerlNum
                PerlSafeInt
                PerlSafeFloat
             BlessedNum[`d]
                BlessedInt[`d]
                BlessedFloat[`d]
             NaN
             Inf[`s]
             FloatSafeNum
                FloatBinary[`b, `e]
                FloatDecimal[`d, `e]
             RealNum
                RealSafeNum
                   FixedBinary[`b, `s]
                   FixedDecimal[`d, `s]
 
          Value (T:S)
             Str (T:S)
                Char[`b]

=head2 Basic types

=over

=item C<< NumLike >>

Behaves like C<LaxNum> from L<Types::Standard>, but will also accept blessed number types.  Unlike C<StrictNum>, it will accept C<NaN>
and C<Inf> numbers.

=item C<< NumRange[`n, `p] >>

Only accepts numbers within a certain range.  The two parameters are the minimums and maximums, inclusive.

=item C<< PerlNum >>

Exactly like C<LaxNum>, but with a different parent.  Only accepts unblessed numbers.

=item C<< BlessedNum >>

Only accepts blessed numbers.  A blessed number would be using something like L<Math::BigInt> or L<Math::BigFloat>.  It doesn't
directly C<isa> check those classes, just that the number is blessed.

=item C<< BlessedNum[`d] >>

A blessed number that supports at least certain amount of digit accuracy.  The blessed number must support the C<accuracy> or
C<div_scale> method.

For example, C<< BlessedNum[40] >> would work for the default settings of L<Math::BigInt>, and supports numbers at least as big as
128-bit integers.

=item C<< NaN >>

A "not-a-number" value, either embedded into the Perl native float or a blessed C<NaN>, checked via C<is_nan>.

=item C<< Inf >>

An infinity value, either embedded into the Perl native float or a blessed C<Inf>, checked via C<is_inf>.

=item C<< Inf[`s] >>

   Inf['+']
   Inf['-']

An infinity value with a certain sign, either embedded into the Perl native float or a blessed C<Inf>, checked via C<is_inf>.  The
parameter must be a plus or minus character.

=item C<< RealNum >>

Like C<NumLike>, but does not accept NaN or Inf.  Closer to the spirit of C<StrictNum>, but accepts blessed numbers as well.

=back

=head2 Integers

=over

=item C<< IntLike >>

Behaves like C<Int> from L<Types::Standard>, but will also accept blessed number types and integers in E notation.  There are no
expectations of storage limitations here.  (See C<SignedInt> for that.)

=item C<< PerlSafeInt >>

A Perl (unblessed) integer number than can safely hold the integer presented.  This varies between 32-bit and 64-bit versions of Perl.

For example, for most 32-bit versions of Perl, the largest integer than can be safely held in a 4-byte NV (floating point number) is
C<18446744073709551614>.  Numbers can go higher than that, but due to the NV's mantissa length (accuracy), information is lost beyond
this point.

In this case, C<...614> would pass and C<...615> would fail.

(Technically, the max integer is C<...615>, but we can't tell the difference between C<...615> and C<...616>, so the cut
off point is C<...614>, inclusive.)

=item C<< BlessedInt >>

A blessed number than is holding an integer.  (A L<Math::BigFloat> with an integer value would still pass.)

=item C<< BlessedInt[`d] >>

A blessed number holding an integer of at most C<`d> digits (inclusive).  The blessed number container must also have digit accuracy
to support this number.  (See C<< BlessedNum[`d] >>.)

=item C<< SignedInt >>

A signed integer (blessed or otherwise) that can safely hold its own number.  This is different than C<IntLike>, which doesn't check
for storage limitations.

=item C<< SignedInt[`b] >>

A signed integer that can hold a C<`b> bit number and is within those boundaries.  One bit is reserved for the sign, so the max limit
on a 32-bit integer is actually C<< 2**31-1 >> or C<2147483647>.

=item C<< UnsignedInt >>

Like C<SignedInt>, but with a minimum boundary of zero.

=item C<< UnsignedInt[`b] >>

Like C<< SignedInt[`b] >>, but for unsigned integers.  Also, unsigned integers gain their extra bit, so the maximum is twice as high.

=back

=head2 Floating-point numbers

=over

=item C<< PerlSafeFloat >>

A Perl native float that is in the "integer safe" range, or is a NaN/Inf value.

This doesn't guarantee that every single fractional number is going to retain all of its information here.  It only guarantees that
the whole number will be retained, even if the fractional part is partly or completely lost.

=item C<< BlessedFloat >>

A blessed number that will support fractional numbers.  A L<Math::BigFloat> number will pass, whereas a L<Math::BigInt> number will
fail.  However, if that L<Math::BigInt> number is capable of upgrading to a L<Math::BigFloat>, it will pass.

=item C<< BlessedFloat[`d] >>

A float-capable blessed number that supports at least certain amount of digit accuracy.  The number itself is not boundary checked, as
it is excessively difficult to figure out the exact dimensions of a floating point number.  It would also not be useful for numbers
like C<0.333333...> to fail checks.

=item C<< FloatSafeNum >>

A Union of C<PerlSafeFloat> and C<BlessedFloat>.  In other words, a float-capable number with some basic checks to make sure
information is retained.

=item C<< FloatBinary[`b, `e] >>

A floating-point number that can hold a C<`b> bit number with C<`e> bits of exponent, and is within those boundaries (or is NaN/Inf).
The bit breakdown follows traditional IEEE 754 floating point standards.  For example:

   FloatBinary[32, 8] =
      32 bits total (`b)
      23 bit  mantissa (significand precision)
       8 bit  exponent (`e)
       1 bit  sign (+/-)

Unlike the C<*Int> types, if Perl's native number cannot support all dimensions of the floating-point number without losing
information, then unblessed numbers are completely off the table.  For example, assuming a 32-bit machine:

   UnsignedInt[64]->check( 0 )        # pass
   UnsignedInt[64]->check( 2 ** 30 )  # pass
   UnsignedInt[64]->check( 2 ** 60 )  # fail, because 32-bit NVs can't safely hold it

   FloatBinary[64, 11]->check( 0 )    # fail
   FloatBinary[64, 11]->check( $any_unblessed_number )  # fail

=item C<< FloatDecimal[`d, `e] >>

A floating-point number that can hold a C<`d> digit number with C<`e> digits of exponent.  Modeled after the IEEE 754 "decimal" float.
Rejects all Perl NVs that won't support the dimensions.  (See C<< FloatBinary[`b, `e] >>.)

=back

=head2 Fixed-point numbers

=over

=item C<< RealSafeNum >>

Like C<FloatSafeNum>, but rejects any NaN/Inf.

=item C<< FixedBinary[`b, `s] >>

A fixed-point number, represented as a C<`b> bit integer than has been shifted by C<`s> digits.  For example, a
C<< FixedBinary[32, 4] >> has a max of C<< 2**31-1 / 10**4 = 214748.3647 >>.  Because integers do not hold NaN/Inf, this type fails
on those.

Otherwise, it has the same properties and caveats as the parameterized C<< Float* >> types.

=item C<< FixedDecimal[`d, `s] >>

Like C<< FixedBinary[`b, `s] >>, but for a C<`d> digit integer.  Or, you could think of C<`d> and C<`s> as accuracy (significant
figures) and decimal precision, respectively.

=back

=head2 Characters

Characters are basically encoded numbers, so there's a few types here.  If you need types that handle multi-length strings, you're
better off using L<Types::Encoding>.

=over

=item C<< Char >>

A single character.  Unicode is supported, but it must be decoded first.  A multi-byte character that Perl thinks is two separate
characters will fail this type.

=item C<< Char[`b] >>

A single character that fits within C<`b> bits.  Unicode is supported, but it must be decoded first.

Also, be aware of the ambiguous nature of 8-bit ASCII characters vs. UTF8:

   use Encode qw(encode decode);

   my $char = 'ђ';
   Char[8]->check($char);   # pass
   print ord($char);        # 209

   $char = decode("UTF-8", $char);
   Char[8]->check($char);   # fail
   print ord($char);        # 1106
   print $char;             # ђ

To mitigate this effect, consider an intersection with C<Chars> and possibly a C<Decode> coercion (both from L<Types::Encoding>).

=back

=head1 AVAILABILITY

The project homepage is L<https://github.com/SineSwiper/Types-Numbers/wiki>.

The latest version of this module is available from the Comprehensive Perl
Archive Network (CPAN). Visit L<http://www.perl.com/CPAN/> to find a CPAN
site near you, or see L<https://metacpan.org/module/Types::Numbers/>.

=for :stopwords cpan testmatrix url annocpan anno bugtracker rt cpants kwalitee diff irc mailto metadata placeholders metacpan

=head1 SUPPORT

=head2 Internet Relay Chat

You can get live help by using IRC ( Internet Relay Chat ). If you don't know what IRC is,
please read this excellent guide: L<http://en.wikipedia.org/wiki/Internet_Relay_Chat>. Please
be courteous and patient when talking to us, as we might be busy or sleeping! You can join
those networks/channels and get help:

=over 4

=item *

irc.perl.org

You can connect to the server at 'irc.perl.org' and talk to this person for help: SineSwiper.

=back

=head2 Bugs / Feature Requests

Please report any bugs or feature requests via L<https://github.com/SineSwiper/Types-Numbers/issues>.

=head1 AUTHOR

Brendan Byrd <BBYRD@CPAN.org>

=head1 CONTRIBUTOR

Brendan Byrd <bbyrd@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2013 by Brendan Byrd.

This is free software, licensed under:

  The Artistic License 2.0 (GPL Compatible)

=cut
