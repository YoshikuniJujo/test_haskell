{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Angle where

import Foreign.C.Types

data Angle = Radian_ CDouble | Degree_ CDouble deriving Show

{-# COMPLETE Radian #-}

pattern Radian :: CDouble -> Angle
pattern Radian r <- (radian -> r) where Radian = Radian_

radian :: Angle -> CDouble
radian = \case Radian_ r -> r; Degree_ d -> d / 360 * 2 * pi

{-# COMPLETE Degree #-}

pattern Degree :: CDouble -> Angle
pattern Degree d <- (degree -> d) where Degree = Degree_

degree :: Angle -> CDouble
degree = \case Radian_ r -> r / (2 * pi) * 360; Degree_ d -> d
