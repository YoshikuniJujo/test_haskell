{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Angle where

import Text.Read

data Angle f = Radian_ f | Degree_ f

{-# COMPLETE Radian #-}

pattern Radian :: Floating f => f -> Angle f
pattern Radian r <- (radian -> r) where Radian = Radian_

radian :: Floating f => Angle f -> f
radian = \case Radian_ r -> r; Degree_ d -> d / 360 * 2 * pi

{-# COMPLETE Degree #-}

pattern Degree :: Floating f => f -> Angle f
pattern Degree d <- (degree -> d) where Degree = Degree_

degree :: Floating f => Angle f -> f
degree = \case Radian_ r -> r / (2 * pi) * 360; Degree_ d -> d

instance Show f => Show (Angle f) where
	showsPrec d = \case
		Radian_ x -> showParen (d > 10)
			$ ("Radian " ++) . showsPrec 11 x
		Degree_ x -> showParen (d > 10)
			$ ("Degree " ++) . showsPrec 11 x

instance (Read f, Floating f) => Read (Angle f) where
	readPrec = parens $ prec 10 do
			Ident "Radian" <- lexP
			f <- step readPrec
			pure $ Radian_ f
		+++ prec 10 do
			Ident "Degree" <- lexP
			f <- step readPrec
			pure $ Degree_ f

instance (Eq f, Floating f) => Eq (Angle f) where
	Degree_ x == Degree_ y = x == y
	Radian x == Radian y = x == y

instance (Ord f, Floating f) => Ord (Angle f) where
	Degree_ x <= Degree_ y = x <= y
	Radian x <= Radian y = x <= y
