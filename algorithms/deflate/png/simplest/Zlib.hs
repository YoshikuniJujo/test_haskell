{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Zlib (header, encodeHeader, Cmf(..), FLvl(..), FDct(..)) where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

data Cmf = Deflate Int | CmfRaw Word8 deriving Show

data FLvl = FLvl Word8 deriving Show

data FDct = FDct Adler32 deriving Show

data Adler32 = Adler32 Word32 deriving Show

header :: StateT BS.ByteString Maybe (Cmf, FLvl, Maybe FDct)
header = do
	(c, w) <- item 1 cmf
	(b, l) <- item 1 . flg $ fromIntegral w
	md <- if b
		then (Just <$>) . item 4
			$ ((Just . FDct . Adler32) .) . flip BS.foldl' 0 . curry
			$ uncurry (.|.) . ((`shiftL` 8) *** fromIntegral)
		else return Nothing
	return (c, l, md)

encodeHeader :: (Cmf, FLvl, Maybe FDct) -> BS.ByteString
encodeHeader (c, l, Nothing) = let (cbs, cw) = encodeCmf c in
	cbs `BS.append` encodeFlg cw l
encodeHeader _ = error "Zlib.encodeHeader: not implemented"

cmf :: BS.ByteString -> Maybe (Cmf, Word8)
cmf bs = case BS.uncons bs of
	Just (w, "")
		| w .&. 0x0f == 8 -> Just (Deflate $ 2 ^ (w `shiftR` 4 + 8), w)
		| otherwise -> Just (CmfRaw w, w)
	_ -> Nothing

encodeCmf :: Cmf -> (BS.ByteString, Word16)
encodeCmf (Deflate s) = (BS.singleton w, fromIntegral w)
	where w = fromIntegral (log2 s - 8) `shiftL` 4 .|. 8
encodeCmf (CmfRaw w) = (BS.singleton w, fromIntegral w)

log2 :: Integral n => n -> n
log2 n | n < 2 = 0
log2 n = 1 + log2 (n `div` 2)

flg :: Word16 -> BS.ByteString -> Maybe (Bool, FLvl)
flg c bs = case BS.uncons bs of
	Just (w, "") -> case (c `shiftL` 8 .|. fromIntegral w) `mod` 31 of
		0 -> Just (w `testBit` 5, FLvl $ w `shiftR` 6)
		_ -> Nothing
	_ -> Nothing

encodeFlg :: Word16 -> FLvl -> BS.ByteString
encodeFlg c (FLvl l) = BS.singleton . to31 c $ l `shiftL` 6

to31 :: Word16 -> Word8 -> Word8
to31 c w = w .|. fromIntegral n
	where
	n = 31 - (c `shiftL` 8 .|. fromIntegral w) `mod` 31

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)
