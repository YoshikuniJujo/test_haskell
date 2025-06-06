{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools.ByteStringNum (numToBs, bsToNum, numToBs') where

import Data.Bits
import Data.ByteString qualified as BS

numToBs :: (Bits n, Integral n) => n -> BS.ByteString
numToBs 0 = ""
numToBs n = fromIntegral (n .&. 0xff) `BS.cons` numToBs (n `shiftR` 8)

numToBs' :: (Bits n, Integral n) => Int -> n -> BS.ByteString
numToBs' ln _ | ln < 1 = ""
numToBs' ln n = fromIntegral (n .&. 0xff) `BS.cons` numToBs' (ln - 1) (n `shiftR` 8)

bsToNum :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum = foldr (\b s -> fromIntegral b .|. s `shiftL` 8) 0 . BS.unpack
