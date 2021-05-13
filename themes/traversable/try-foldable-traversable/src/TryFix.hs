{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryFix where

import Control.Monad.Fix

f, g :: Integer -> Integer
f n | n < 1 = 1
f n = n * f (n - 1)

g = fix \k n -> if n < 1 then 1 else n * k (n - 1)
