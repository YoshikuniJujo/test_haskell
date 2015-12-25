{-# LANGUAGE OverloadedStrings #-}

module YjChunks (
	magic, dend, YjChunks, YjChunk, mkYjChunk, toByteString,
	fromNum, toNum,
	) where

import Data.Bits
import qualified Data.ByteString as BS

magic :: BS.ByteString
magic = "$~\NUL\146\225\214\211b"

dend :: YjChunk
Just dend = mkYjChunk "DEND" ""

type YjChunks = [YjChunk]

data YjChunk = YjChunk {
	typ :: BS.ByteString,
	dat :: BS.ByteString
	} deriving (Show, Eq)

mkYjChunk :: BS.ByteString -> BS.ByteString -> Maybe YjChunk
mkYjChunk t d
	| BS.length t == 4 = Just $ YjChunk t d
	| otherwise = Nothing

toByteString :: (BS.ByteString -> BS.ByteString) -> YjChunk -> (Int, BS.ByteString)
toByteString cs YjChunk { typ = t, dat = d } = let bs = t `BS.append` d in (
	BS.length d,
	bs `BS.append` cs bs )

fromNum :: (Bits n, Integral n) => Int -> n -> BS.ByteString
fromNum 0 _ = ""
fromNum c n = BS.cons (fromIntegral n) $ fromNum (c - 1) (n `shiftR` 8)

toNum :: (Bits n, Num n) => BS.ByteString -> n
toNum bs = case BS.uncons bs of
	Just (w, bs') -> fromIntegral w .|. (toNum bs' `shiftL` 8)
	_ -> 0
