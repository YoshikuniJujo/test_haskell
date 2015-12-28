{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Zlib (Header, Cmf(..), FLvl(..), FDct(..), encHeader, decHeader) where

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.Word
import qualified Data.ByteString as BS

import Bits
import Item

type Header = (Cmf, FLvl, Maybe FDct)

data Cmf = Deflate Int | CmfRaw Word8 deriving Show

data FLvl = FLvl Word8 deriving Show

data FDct = FDct Word32 deriving Show

decHeader :: BS.ByteString -> Maybe (Header, BS.ByteString)
decHeader = runStateT $ do
	(c, w) <- item 1 cmf
	(b, l) <- item 1 . flg $ fromIntegral w
	md <- if b
		then (Just <$>) . item 4 $ (Just . FDct) . beFromByteString
		else return Nothing
	return (c, l, md)

cmf :: BS.ByteString -> Maybe (Cmf, Word8)
cmf s = case BS.uncons s of
	Just (w, "")
		| w .&. 0x0f == 8 -> Just (Deflate $ 2 ^ (w `shiftR` 4 + 8), w)
		| otherwise -> Just (CmfRaw w, w)
	_ -> Nothing

flg :: Word16 -> BS.ByteString -> Maybe (Bool, FLvl)
flg c s = case BS.uncons s of
	Just (w, "") | 0 <- (c `shiftL` 8 .|. fromIntegral w) `mod` 31 ->
		Just (w `testBit` 5, FLvl $ w `shiftR` 6)
	_ -> Nothing

encHeader :: Header -> BS.ByteString
encHeader (c, l, Nothing) = uncurry ((. (`encFlg` l)) . BS.append) $ encCmf c
encHeader _ = error "Zlib.encHeader: not implemented"

encCmf :: Cmf -> (BS.ByteString, Word16)
encCmf (Deflate s) = (BS.singleton w, fromIntegral w)
	where
	w = (log2 s - 8) `shiftL` 4 .|. 8
	log2 n | n < 2 = 0; log2 n = 1 + log2 (n `div` 2)
encCmf (CmfRaw w) = (BS.singleton w, fromIntegral w)

encFlg :: Word16 -> FLvl -> BS.ByteString
encFlg c (FLvl l) = BS.singleton . ($ l `shiftL` 6) $ (.|.) <$> id <*>
	fromIntegral . (31 -) . (`mod` 31) . (c `shiftL` 8 .|.) . fromIntegral
