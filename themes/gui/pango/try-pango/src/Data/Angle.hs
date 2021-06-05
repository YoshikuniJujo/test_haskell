{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Angle where

data Angle f = Radian_ f | Degree_ f deriving Show

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
