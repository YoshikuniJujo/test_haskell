{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Array.Tools where

import Data.Array.MArray

{-# INLINE copy #-}
copy :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
copy a d s = writeArray a d =<< readArray a s
