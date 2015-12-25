{-# LANGUAGE OverloadedStrings #-}

module YjChunks (YjChunks, YjChunk, yjChunk, encode, magic, dend) where

import qualified Data.ByteString as BS

type YjChunks = [YjChunk]

data YjChunk = YjChunk {
	typ :: BS.ByteString,
	dat :: BS.ByteString
	} deriving (Show, Eq)

yjChunk :: BS.ByteString -> BS.ByteString -> Maybe YjChunk
yjChunk "DEND" d | not $ BS.null d = Nothing
yjChunk t d	| BS.length t == 4 = Just $ YjChunk t d
		| otherwise = Nothing

encode :: (BS.ByteString -> BS.ByteString) -> YjChunk -> (Int, BS.ByteString)
encode cs yc = (BS.length $ dat yc, bs `BS.append` cs bs)
	where bs = typ yc `BS.append` dat yc

magic :: BS.ByteString
magic = "$~\NUL\146\225\214\211b"

dend :: YjChunk
Just dend = yjChunk "DEND" ""
