{-# LANGUAGE TupleSections, OverloadedStrings, PackageImports #-}

module Chunks (Chunks, Chunk(..), encode, decode, idat) where

import Control.Applicative ((<$>))
import "monads-tf" Control.Monad.State (evalStateT, unless)
import Data.Bool (bool)
import qualified Data.ByteString as BS (
	ByteString, length, append, concat, splitAt)

import CRC (check, calc)
import Item (item, untilM)
import Bits (beFromByteString, beToByteString)

magic :: BS.ByteString
magic = "\x89PNG\r\n\SUB\n"

type Chunks = [Chunk]

data Chunk = Chunk { typ :: BS.ByteString, dat :: BS.ByteString }
	deriving (Show, Eq)

idat :: Chunks -> BS.ByteString
idat = BS.concat . map dat . filter ((== "IDAT") . typ)

encode :: Chunks -> BS.ByteString
encode cs = (magic `BS.append`) . BS.concat . (<$> cs ++ [Chunk "IEND" ""]) $
	\c -> BS.concat [
		beToByteString 4 . BS.length $ dat c, typ c, dat c,
		beToByteString 4 . calc $ typ c `BS.append` dat c ]

decode :: BS.ByteString -> Maybe Chunks
decode = evalStateT $ item 8 (bool Nothing (Just ()) . (== magic)) >>
	untilM (== Chunk "IEND" "") chunk
	where chunk = do
		td <- (`item` Just) . (4 +) =<< item 4 (Just . beFromByteString)
		cs <- item 4 $ Just . beFromByteString
		unless (check td cs) $ fail "error"
		return . uncurry Chunk $ BS.splitAt 4 td
