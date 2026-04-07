{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Foldable

times_ :: (Enum n, Num n) => n -> (n -> IO a) -> IO ()
times_ n = for_ [0 .. n - 1]
