{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Bits
import Data.ByteString qualified as BS

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

num32ToBs :: (Bits n, Integral n) => n -> BS.ByteString
num32ToBs n = BS.replicate (4 - BS.length bs) 0 `BS.append` bs
	where
	bs = BS.pack $ be [] n
	be ws = \case
		0 -> ws
		n -> be ((fromIntegral $ n .&. 0xff) : ws) (n `shiftR` 8)
