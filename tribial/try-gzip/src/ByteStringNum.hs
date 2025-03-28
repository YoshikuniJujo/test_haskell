{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ByteStringNum where

import Data.Bits
import Data.ByteString qualified as BS

numToBs :: (Bits n, Integral n) => n -> BS.ByteString
numToBs 0 = ""
numToBs n = fromIntegral (n .&. 0xff) `BS.cons` numToBs (n `shiftR` 8)
