{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bits.ToolsYj (checkBits) where

import Data.Bits

checkBits :: Bits bs => bs -> bs -> Bool
checkBits wnt = (== wnt) . (.&. wnt)
