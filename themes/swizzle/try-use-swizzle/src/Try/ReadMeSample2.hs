{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.ReadMeSample2 where

import GHC.Generics
import Data.Tuple
import Data.Swizzle qualified as Swz
import Data.Swizzle.Class

newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show
newtype Alpha = Alpha Double deriving Show

data Argb = Argb Alpha Red Green Blue deriving (Show, Generic)

instance Swizzle1 Argb where type X Argb = Alpha
instance Swizzle2 Argb where type Y Argb = Red
instance Swizzle3 Argb where type Z Argb = Green
instance Swizzle4 Argb where type W Argb = Blue

sample :: Argb
sample = Argb (Alpha 0.5) (Red 0.9) (Green 0.3) (Blue 0.2)

red :: Solo Red
red = Swz.y sample

alphaGreen :: (Alpha, Green)
alphaGreen = Swz.xz sample

rgba :: (Red, Green, Blue, Alpha)
rgba = Swz.yzwx sample

