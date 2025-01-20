{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Color where

import GHC.Generics
import Data.Tuple
import Data.SwizzleSet qualified as SwzS
import Data.SwizzleSet.Class

newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show
newtype Alpha = Alpha Double deriving Show

data Argb = Argb Alpha Red Green Blue deriving (Show, Generic)

instance SwizzleSet1 Argb where type X Argb = Alpha
instance SwizzleSet2 Argb where type Y Argb = Red
instance SwizzleSet3 Argb where type Z Argb = Green
instance SwizzleSet4 Argb where type W Argb = Blue

sample :: Argb
sample = Argb (Alpha 0.5) (Red 0.9) (Green 0.3) (Blue 0.2)

lessRed :: Argb
lessRed = SwzS.y (Red 0.4) sample -- Argb (Alpha 0.5) (Red 0.4) (Green 0.3) (Blue 0.2)

lessAlphaGreen :: Argb
lessAlphaGreen = SwzS.xz (Alpha 0.2, Green 0.1) sample
	-- Argb (Alpha 0.2) (Red 0.4) (Green 0.1) (Bluye 0.2)
