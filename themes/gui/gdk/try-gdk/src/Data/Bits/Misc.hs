{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bits.Misc (separateBits) where

import Data.Bits

separateBits :: Bits n => Int -> n -> [n]
separateBits c n = filter (/= zeroBits) $ (\i -> n .&. bit i) <$> [0 .. c - 1]
