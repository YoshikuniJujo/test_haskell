{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Color where

import GHC.Generics
import Control.Lens

import Data.SwizzleLens as SwzL
import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS

newtype Alpha = Alpha Double deriving Show
newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show

data Argb = Argb Alpha Red Green Blue deriving (Show, Generic)

instance Swz.Swizzle1 Argb where type X Argb = Alpha
instance Swz.Swizzle2 Argb where type Y Argb = Red
instance Swz.Swizzle3 Argb where type Z Argb = Green
instance Swz.Swizzle4 Argb where type W Argb = Blue

instance SwzS.SwizzleSet1 Argb Alpha where type X Argb Alpha = Argb
instance SwzS.SwizzleSet2 Argb Red where type Y Argb Red = Argb
instance SwzS.SwizzleSet3 Argb Green where type Z Argb Green = Argb
instance SwzS.SwizzleSet4 Argb Blue where type W Argb Blue = Argb

sample :: Argb
sample = Argb (Alpha 0.8) (Red 0.3) (Green 0.8) (Blue 0.1)

foo :: (Red, Blue)
foo = sample ^. SwzL.yw

bar :: Argb
bar = sample & SwzL.xz .~ (Alpha 0.5, Green 0.9)
