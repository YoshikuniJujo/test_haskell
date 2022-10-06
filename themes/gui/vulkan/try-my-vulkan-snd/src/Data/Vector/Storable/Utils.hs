{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Vector.Storable.Utils where

import Foreign.Storable

import qualified Data.Vector.Storable as V

genericTake :: (Integral i, Storable a) => i -> V.Vector a -> V.Vector a
genericTake = V.take . fromIntegral
