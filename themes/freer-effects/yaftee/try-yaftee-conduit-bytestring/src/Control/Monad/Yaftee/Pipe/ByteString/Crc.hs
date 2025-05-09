{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Crc (

	crc32, compCrc32, Crc32(..), crc32ToByteString

	) where

import Control.Arrow
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Array
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

type Pkg = "try-yaftee-conduit-bytestring"

crc32 :: (U.Member Pipe.P es, U.Member (State.Named Pkg Crc32) es) =>
	Eff.E es BS.ByteString BS.ByteString r
crc32 = do
	State.putN Pkg $ Crc32 0xffffffff
	crc32Body

compCrc32 :: U.Member (State.Named Pkg Crc32) es => Eff.E es i o ()
compCrc32 = State.modifyN Pkg \(Crc32 c) -> Crc32 $ complement c

crc32Body :: (U.Member Pipe.P es, U.Member (State.Named Pkg Crc32) es) =>
	Eff.E es BS.ByteString BS.ByteString r
crc32Body = fix \go -> Pipe.await >>= \bs -> do
	State.modifyN Pkg \(Crc32 c) -> Crc32 $ c `stepBS` bs
	Pipe.yield bs
	go

newtype Crc32 = Crc32 Word32 deriving Show

crc32ToByteString :: Crc32 -> BS.ByteString
crc32ToByteString (Crc32 c) = BS.replicate (4 - BS.length bs) 0 `BS.append` bs
	where
	bs = numToBs c
	numToBs 0 = ""
	numToBs n = fromIntegral (n .&. 0xff) `BS.cons` numToBs (n `shiftR` 8)

popBit :: Bits b => b -> (Bool, b)
popBit n = (n `testBit` 0, n `shiftR` 1)

crc1 :: Word32 -> Word32
crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit

crc8 :: Word8 -> Word32
crc8 n = iterate crc1 (fromIntegral n) !! 8

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte n = (fromIntegral n, n `shiftR` 8)

step8 :: Word32 -> Word8 -> Word32
step8 n b = uncurry xor . (first $ (table !) . (`xor` b)) $ popByte n

stepBS :: Word32 -> BS.ByteString -> Word32
stepBS n = BS.foldl' step8 n
