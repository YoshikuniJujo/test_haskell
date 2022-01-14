{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Flip where

import Data.List

import Data.Curry
import Data.Swizzle.TH

concat <$> swizzle `mapM` permutations "xyz"

flip13 :: (a -> b -> c -> r) -> c -> b -> a -> r
flip13 f = crr3 $ unc3 f . zyx

flip' :: (b -> c -> a -> r) -> a -> b -> c -> r
flip' f = crr3 $ unc3 f . yzx

flip'' :: (c -> a -> b -> r) -> a -> b -> c -> r
flip'' f = crr3 $ unc3 f . zxy
