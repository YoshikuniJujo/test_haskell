{-# LANGUAGE TupleSections, OverloadedStrings, PackageImports #-}

module Zlib (Header, Cmf(..), FLvl(..), FDct(..), encode, decode) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (replicateM, guard)
import "monads-tf" Control.Monad.State (runStateT)
import Data.Word (Word8, Word16, Word32)
import qualified Data.ByteString as BS (ByteString, singleton, append)

import Bits (beFromByteString, testBit, shiftL, shiftR, (.&.), (.|.))
import Item (item, byte)

type Header = (Cmf, FLvl, Maybe FDct)

data Cmf = Deflate Int | CmfRaw Word8 deriving Show

data FLvl = FLvl Word8 deriving Show

data FDct = FDct Word32 deriving Show

encode :: Header -> BS.ByteString
encode (cm, FLvl l, Nothing) = uncurry ((. BS.singleton . flg) . BS.append)
	(BS.singleton &&& fromIntegral $ cmf cm :: (BS.ByteString, Word16))
	where
	cmf (Deflate s) = (log2 s - 8) `shiftL` 4 .|. 8
	cmf (CmfRaw w) = w
	flg w = (.|.) <$> id <*> fromIntegral . (31 -)
		. (`mod` 31) . (w `shiftL` 8 .|.) . fromIntegral $ l `shiftL` 6
	log2 n | n < 2 = 0; log2 n = 1 + log2 (n `div` 2)
encode _ = error "Zlib.encode: not implemented"

decode :: BS.ByteString -> Maybe (Header, BS.ByteString)
decode = runStateT $ do
	[c, f] <- replicateM 2 $ item 1 byte
	guard $ (== (0 :: Word16)) . (`mod` 31) $
		fromIntegral c `shiftL` 8 .|. fromIntegral f
	md <- if f `testBit` 5
		then (Just <$>) . item 4 $ (Just . FDct) . beFromByteString
		else return Nothing
	return . (, FLvl $ f `shiftR` 6, md) $ if c .&. 0x0f == 8
		then Deflate $ 2 ^ (c `shiftR` 4 + 8) else CmfRaw c
