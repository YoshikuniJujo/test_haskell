{-# LANGUAGE ImportQualifiedPost #-}

module Data.Image.Simple where

import Control.Monad.Primitive
import Data.Vector
import Data.Vector.Generic.Mutable qualified as V
import Data.Word

data I s = I { width :: Int, height :: Int, body :: MVector s Word8 }

new :: PrimMonad m => Int -> Int -> m (I (PrimState m))
new w h = I w h <$> V.new (w * h)
