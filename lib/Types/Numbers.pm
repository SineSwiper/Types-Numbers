package Types::Numbers;

# VERSION
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
use Math::BigFloat v1.65;  # earliest version that passes tests
use Data::Float;
use Data::Integer;

use constant {
   _BASE2_LOG => log(2) / log(10),
};

sub _croak ($;@) { require Type::Exception; goto \&Type::Exception::croak }

no warnings;  # don't warn on type checks

#############################################################################
# Basic globals

my $bigtwo = Math::BigFloat->new(2);
my $bigten = Math::BigFloat->new(10);

# Large 64-bit floats (long doubles) tend to stringify themselves in exponent notation, even
# though the number is still pristine.  IOW, the numeric form is perfect, but the string form
# loses information.  This can be a problem for stringified inlines.
my @df_max_int_parts = Data::Float::float_parts( Data::Float::max_integer );
my $DF_MAX_INT = $bigtwo->copy->bpow($df_max_int_parts[1])->bmul($df_max_int_parts[2])->as_int;

my $SAFE_NUM_MIN = Math::BigInt->new(
   Data::Integer::min_signed_natint   < $DF_MAX_INT * -1 ?
   Data::Integer::min_signed_natint   : $DF_MAX_INT * -1
);
my $SAFE_NUM_MAX = Math::BigInt->new(
   Data::Integer::max_unsigned_natint > $DF_MAX_INT *  1 ?
   Data::Integer::max_unsigned_natint : $DF_MAX_INT *  1,
);

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

# we need to optimize out all of the NumLike checks
my $_NumRange_perlsafe = Type::Tiny->new(
   display_name => "_NumRange_perlsafe",
   parent     => $_NumLike,
   # no equals because MAX+1 = MAX after truncation
   constraint => sub { $_ > $SAFE_NUM_MIN and $_ < $SAFE_NUM_MAX },
   inlined    => sub { "$_[1] > ".$SAFE_NUM_MIN." and $_[1] < ".$SAFE_NUM_MAX },
);

### XXX: This string equality check is necessary because Math::BigInt seems to think 1.5 == 1.
### However, this is problematic with long doubles that stringify into E notation.
my $_IntLike = $meta->add_type(
   name       => 'IntLike',
   parent     => $_NumLike,
   library    => __PACKAGE__,
   constraint => sub { /\d+/ and int($_) == $_ and (int($_) eq $_ or not ref($_)) },
   inlined    => sub {
      my ($self, $val) = @_;
      $self->parent->inline_check($val)." and $val =~ /\\d+/ and int($val) == $val and (int($val) eq $val or not ref($val))";
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
   inlined    => sub {
      my $val = $_[1];
      "!ref($val) and $val =~ /\\d+/ and int($val) == $val and ".$_NumRange_perlsafe->inline_check($val);
   },
) );

my $_BlessedInt = $meta->add_type( Type::Tiny::Intersection->new(
   library    => __PACKAGE__,
   type_constraints => [ $_BlessedNum, $_IntLike ],
)->create_child_type(
   name       => 'BlessedInt',
   library    => __PACKAGE__,
   inlined    => sub {
      my $val = $_[1];
      Types::Standard::Object->inline_check($val)." and $val =~ /\\d+/ and int($val) == $val and int($val) eq $val";
   },
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
      my $perlsafe_inline = $min >= $SAFE_NUM_MIN && $max <= $SAFE_NUM_MAX ?
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

=encoding utf8

=begin wikidoc

= DESCRIPTION

Because we deal with numbers every day in our programs and modules, this is an extensive [Type::Tiny] library of number validations.
Like [Type::Tiny], these types work with all modern OO platforms and as a standalone type system.

= TYPES

== Overview

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

== Basic types

=== NumLike

Behaves like {LaxNum} from [Types::Standard], but will also accept blessed number types.  Unlike {StrictNum}, it will accept {NaN}
and {Inf} numbers.

=== NumRange[`n, `p]

Only accepts numbers within a certain range.  The two parameters are the minimums and maximums, inclusive.

=== PerlNum

Exactly like {LaxNum}, but with a different parent.  Only accepts unblessed numbers.

=== BlessedNum

Only accepts blessed numbers.  A blessed number would be using something like [Math::BigInt] or [Math::BigFloat].  It doesn't
directly {isa} check those classes, just that the number is blessed.

=== BlessedNum[`d]

A blessed number that supports at least certain amount of digit accuracy.  The blessed number must support the {accuracy} or
{div_scale} method.

For example, {BlessedNum[40]} would work for the default settings of [Math::BigInt], and supports numbers at least as big as
128-bit integers.

=== NaN

A "not-a-number" value, either embedded into the Perl native float or a blessed {NaN}, checked via {is_nan}.

=== Inf

An infinity value, either embedded into the Perl native float or a blessed {Inf}, checked via {is_inf}.

=== Inf[`s]

   Inf['+']
   Inf['-']

An infinity value with a certain sign, either embedded into the Perl native float or a blessed {Inf}, checked via {is_inf}.  The
parameter must be a plus or minus character.

=== RealNum

Like [/NumLike], but does not accept NaN or Inf.  Closer to the spirit of {StrictNum}, but accepts blessed numbers as well.

== Integers

=== IntLike

Behaves like {Int} from [Types::Standard], but will also accept blessed number types and integers in E notation.  There are no
expectations of storage limitations here.  (See [/SignedInt] for that.)

=== PerlSafeInt

A Perl (unblessed) integer number than can safely hold the integer presented.  This varies between 32-bit and 64-bit versions of Perl.

For example, for most 32-bit versions of Perl, the largest integer than can be safely held in a 4-byte NV (floating point number) is
{9007199254740992}.  Numbers can go higher than that, but due to the NV's mantissa length (accuracy), information is lost beyond
this point.

In this case, {...992} would pass and {...993} would fail.

(Technically, the max integer is {...993}, but we can't tell the difference between {...993} and {...994}, so the cut
off point is {...992}, inclusive.)

Be aware that Perls compiled with "long doubles" have a unique problem with storage and information loss: their number form maintains
accuracy while their (default) stringified form loses information.  For example, take the max safe integer for a long double:

   $num = 18446744073709551615;
   say $num;                 # 1.84467440737095516e+19
   say $num == 18446744073709551615;  # true, so the full number is still there
   say sprintf('%u', $num);  # 18446744073709551615

These numbers are considered safe for storage.  If this is not preferred, consider a simple {/e/} check for stringified E notation.

=== BlessedInt

A blessed number than is holding an integer.  (A [Math::BigFloat] with an integer value would still pass.)

=== BlessedInt[`d]

A blessed number holding an integer of at most {`d} digits (inclusive).  The blessed number container must also have digit accuracy
to support this number.  (See [/BlessedNum[`d]].)

=== SignedInt

A signed integer (blessed or otherwise) that can safely hold its own number.  This is different than {IntLike}, which doesn't check
for storage limitations.

=== SignedInt[`b]

A signed integer that can hold a {`b} bit number and is within those boundaries.  One bit is reserved for the sign, so the max limit
on a 32-bit integer is actually {2**31-1} or {2147483647}.

=== UnsignedInt

Like [/SignedInt], but with a minimum boundary of zero.

=== UnsignedInt[`b]

Like [/SignedInt[`b]], but for unsigned integers.  Also, unsigned integers gain their extra bit, so the maximum is twice as high.

== Floating-point numbers

=== PerlSafeFloat

A Perl native float that is in the "integer safe" range, or is a NaN/Inf value.

This doesn't guarantee that every single fractional number is going to retain all of its information here.  It only guarantees that
the whole number will be retained, even if the fractional part is partly or completely lost.

=== BlessedFloat

A blessed number that will support fractional numbers.  A [Math::BigFloat] number will pass, whereas a [Math::BigInt] number will
fail.  However, if that [Math::BigInt] number is capable of upgrading to a [Math::BigFloat], it will pass.

=== BlessedFloat[`d]

A float-capable blessed number that supports at least certain amount of digit accuracy.  The number itself is not boundary checked, as
it is excessively difficult to figure out the exact dimensions of a floating point number.  It would also not be useful for numbers
like {0.333333...} to fail checks.

=== FloatSafeNum

A Union of [/PerlSafeFloat] and [/BlessedFloat].  In other words, a float-capable number with some basic checks to make sure
information is retained.

=== FloatBinary[`b, `e]

A floating-point number that can hold a {`b} bit number with {`e} bits of exponent, and is within those boundaries (or is NaN/Inf).
The bit breakdown follows traditional IEEE 754 floating point standards.  For example:

   FloatBinary[32, 8] =
      32 bits total (`b)
      23 bit  mantissa (significand precision)
       8 bit  exponent (`e)
       1 bit  sign (+/-)

Unlike the {*Int} types, if Perl's native number cannot support all dimensions of the floating-point number without losing
information, then unblessed numbers are completely off the table.  For example, assuming a 32-bit machine:

   (UnsignedInt[64])->check( 0 )        # pass
   (UnsignedInt[64])->check( 2 ** 30 )  # pass
   (UnsignedInt[64])->check( 2 ** 60 )  # fail, because 32-bit NVs can't safely hold it

   (FloatBinary[64, 11])->check( 0 )    # fail
   (FloatBinary[64, 11])->check( $any_unblessed_number )  # fail

=== FloatDecimal[`d, `e]

A floating-point number that can hold a {`d} digit number with {`e} digits of exponent.  Modeled after the IEEE 754 "decimal" float.
Rejects all Perl NVs that won't support the dimensions.  (See [/FloatBinary[`b, `e]].)

== Fixed-point numbers

=== RealSafeNum

Like [/FloatSafeNum], but rejects any NaN/Inf.

=== FixedBinary[`b, `s]

A fixed-point number, represented as a {`b} bit integer than has been shifted by {`s} digits.  For example, a
{FixedBinary[32, 4]} has a max of {2**31-1 / 10**4 = 214748.3647}.  Because integers do not hold NaN/Inf, this type fails
on those.

Otherwise, it has the same properties and caveats as the parameterized {Float*} types.

=== FixedDecimal[`d, `s]

Like [/FixedBinary[`b, `s]], but for a {`d} digit integer.  Or, you could think of {`d} and {`s} as accuracy (significant
figures) and decimal precision, respectively.

== Characters

Characters are basically encoded numbers, so there's a few types here.  If you need types that handle multi-length strings, you're
better off using [Types::Encodings].

=== Char

A single character.  Unicode is supported, but it must be decoded first.  A multi-byte character that Perl thinks is two separate
characters will fail this type.

=== Char[`b]

A single character that fits within {`b} bits.  Unicode is supported, but it must be decoded first.

=end wikidoc
