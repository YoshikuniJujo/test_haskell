{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Color where

import GHC.Generics
import Data.SwizzleSet qualified as SwzS
import Data.SwizzleSet.Class

newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show
newtype Alpha = Alpha Double deriving Show

data Argb = Argb Alpha Red Green Blue deriving (Show, Generic)

instance SwizzleSet1 Argb Alpha where type X Argb Alpha = Argb
instance SwizzleSet2 Argb Red where type Y Argb Red = Argb
instance SwizzleSet3 Argb Green where type Z Argb Green = Argb
instance SwizzleSet4 Argb Blue where type W Argb Blue = Argb

sample :: Argb
sample = Argb (Alpha 0.5) (Red 0.9) (Green 0.3) (Blue 0.2)

lessRed :: Argb
lessRed = SwzS.y sample (Red 0.4) -- Argb (Alpha 0.5) (Red 0.4) (Green 0.3) (Blue 0.2)

lessAlphaGreen :: Argb
lessAlphaGreen = SwzS.xz sample (Alpha 0.2, Green 0.1)
	-- Argb (Alpha 0.2) (Red 0.4) (Green 0.1) (Bluye 0.2)

data Rgba = Rgba Red Green Blue Alpha deriving (Show, Generic)

instance SwizzleSet1 Rgba Red where type X Rgba Red = Rgba
instance SwizzleSet2 Rgba Green where type Y Rgba Green = Rgba
instance SwizzleSet3 Rgba Blue where type Z Rgba Blue = Rgba
instance SwizzleSet4 Rgba Alpha where type W Rgba Alpha = Rgba

data Rgb = Rgb Red Green Blue deriving (Show, Generic)

instance SwizzleSet1 Rgb Alpha where
	type X Rgb Alpha = Argb
	x (Rgb r g b) a = Argb a r g b

instance SwizzleSet1 Rgb Red where type X Rgb Red = Rgb
instance SwizzleSet2 Rgb Green where type Y Rgb Green = Rgb
instance SwizzleSet3 Rgb Blue where type Z Rgb Blue = Rgb

instance SwizzleSet4 Rgb Alpha where
	type W Rgb Alpha = Rgba
	w (Rgb r g b) a = Rgba r g b a

sample2 :: Rgb
sample2 = Rgb (Red 0.9) (Green 0.3) (Blue 0.2)
