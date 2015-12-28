{-# LANGUAGE PackageImports, TupleSections, OverloadedStrings #-}

module Chunks (Chunks, Chunk(..), frPng, toPng, idat) where

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.Bool
import qualified Data.ByteString as BS

import CRC (check, calc)
import Item
import Bits

magic :: BS.ByteString
magic = "\x89PNG\r\n\SUB\n"

idat :: [Chunk] -> BS.ByteString
idat = BS.concat . map dat . filter ((== "IDAT") . typ)

type Chunks = [Chunk]

data Chunk = Chunk { typ :: BS.ByteString, dat :: BS.ByteString }
	deriving (Show, Eq)

frPng :: BS.ByteString -> Maybe [Chunk]
frPng = evalStateT $ item 8 (bool Nothing (Just ()) . (== magic)) >>
	untilM (== Chunk "IEND" "") chunk

chunk :: StateT BS.ByteString Maybe Chunk
chunk = do
	td <- (`item` Just) . (4 +) =<< item 4 (Just . beFromByteString)
	cs <- item 4 $ Just . beFromByteString
	unless (check td cs) $ fail "error"
	return . uncurry Chunk $ BS.splitAt 4 td

toPng :: [Chunk] -> BS.ByteString
toPng cs = (magic `BS.append`) . BS.concat . (<$> cs ++ [Chunk "IEND" ""]) $
	\c -> BS.concat [
		beToByteString 4 . BS.length $ dat c, typ c, dat c,
		beToByteString 4 . calc $ typ c `BS.append` dat c ]
