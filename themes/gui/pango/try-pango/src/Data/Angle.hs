{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Angle (Angle, pattern Radian, radian, pattern Degree, degree) where

import Control.Arrow (second)
import Text.Read (Lexeme(..), readPrec, step, parens, prec, lexP, (+++))

---------------------------------------------------------------------------

-- * DATA TYPE AND PATTERN
-- * INSTANCE DEFINITION

---------------------------------------------------------------------------
-- DATA TYPE AND PATTERN
---------------------------------------------------------------------------

data Angle f = Radian_ f | Degree_ f

-- ^ >>> Radian pi
-- Radian 3.141592653589793
-- >>> degree it
-- 180.0
--
-- >>> Degree 180
-- Degree 180.0
--
-- >>> Radian pi + Degree 180
-- Radian 6.283185307179586

{-# COMPLETE Radian #-}

pattern Radian :: Floating f => f -> Angle f
pattern Radian r <- (radian -> r) where Radian = Radian_

radian :: Floating f => Angle f -> f
radian = \case Radian_ r -> r; Degree_ d -> d * pi / 180

{-# COMPLETE Degree #-}

pattern Degree :: Floating f => f -> Angle f
pattern Degree d <- (degree -> d) where Degree = Degree_

degree :: Floating f => Angle f -> f
degree = \case Radian_ r -> r * 180 / pi; Degree_ d -> d

---------------------------------------------------------------------------
-- INSTANCE DEFINITION
---------------------------------------------------------------------------

instance Show f => Show (Angle f) where
	showsPrec d = \case
		Radian_ x -> showParen (d > 10)
			$ ("Radian " ++) . showsPrec 11 x
		Degree_ x -> showParen (d > 10)
			$ ("Degree " ++) . showsPrec 11 x

instance (Read f, Floating f) => Read (Angle f) where
	readPrec = parens $
		prec 10 do Ident "Radian" <- lexP; Radian_ <$> step readPrec
		+++
		prec 10 do Ident "Degree" <- lexP; Degree_ <$> step readPrec

instance (Eq f, Floating f) => Eq (Angle f) where
	Degree_ x == Degree_ y = x == y; Radian x == Radian y = x == y

instance (Ord f, Floating f) => Ord (Angle f) where
	Degree_ x <= Degree_ y = x <= y; Radian x <= Radian y = x <= y

instance Floating f => Num (Angle f) where
	Degree_ x + Degree_ y = Degree_ $ x + y
	Radian x + Radian y = Radian_ $ x + y
	Degree_ x * Degree_ y = Degree_ $ x * y * pi / 180
	Radian x * Radian y = Radian_ $ x * y
	negate = \case Radian_ x -> Radian_ $ - x; Degree_ x -> Degree_ $ - x
	abs = \case Radian_ x -> Radian_ $ abs x; Degree_ x -> Degree_ $ abs x
	signum = \case
		Radian_ x -> Radian_ $ signum x; Degree_ x -> Radian_ $ signum x
	fromInteger = Radian_ . fromInteger

instance Floating f => Fractional (Angle f) where
	recip = \case
		Radian_ x -> Radian_ $ recip x
		Degree_ x -> Degree_ $ recip x * (180 / pi) ^ (2 :: Int)
	fromRational = Radian_ . fromRational

instance (Floating f, Real f) => Real (Angle f) where
	toRational = toRational . radian

instance Floating f => Floating (Angle f) where
	pi = Radian_ pi
	exp = applyAngle exp
	log = applyAngle log
	sin = applyAngle sin
	cos = applyAngle cos
	asin = applyAngle asin
	acos = applyAngle acos
	atan = applyAngle atan
	sinh = applyAngle sinh
	cosh = applyAngle cosh
	asinh = applyAngle asinh
	acosh = applyAngle acosh
	atanh = applyAngle atanh

applyAngle :: Floating f => (f -> f) -> Angle f -> Angle f
applyAngle f = \case
	Radian_ x -> Radian_ $ f x
	Degree_ x -> Degree_ $ f (x * pi / 180) * 180 / pi

instance (Floating f, RealFrac f) => RealFrac (Angle f) where
	properFraction = \case
		Radian_ x -> Radian `second` properFraction x
		Degree_ x -> (Degree_ . (/ pi) . (* 180))
			`second` properFraction (x * pi / 180)
