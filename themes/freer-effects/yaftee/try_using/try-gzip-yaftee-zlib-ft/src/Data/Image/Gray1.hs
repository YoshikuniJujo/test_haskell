{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Gray1 where

import Data.Vector qualified as V
import Data.Word

data G = G { width :: Int, height :: Int, body :: V.Vector Word8 } deriving Show
