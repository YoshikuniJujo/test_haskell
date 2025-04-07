{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Dict where

import Data.Vector.Mutable
import Data.Word

data Dict s = Dict {
	dictIndexRead :: Int,
	dictIndexWrite :: Int,
	dictBody :: MVector s Word8 }

newDict :: PrimMonad m => m (Dict (PrimState m))
newDict = Dict 0 0 <$> new 32768
