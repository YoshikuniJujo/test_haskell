{-# LANGUAGE PackageImports, TupleSections, OverloadedStrings #-}

module Chunks (Chunks, Chunk(..), frPng, toPng, idat) where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.Bool
import qualified Data.ByteString as BS

import CRC (check, calc)
import Bits

magic :: BS.ByteString
magic = "\x89PNG\r\n\SUB\n"

frPng :: BS.ByteString -> Maybe [Chunk]
frPng = evalStateT $ do
	item 8 (bool Nothing (Just ()) . (== magic))
	untilM (== Chunk "IEND" "") chunk

toPng :: [Chunk] -> BS.ByteString
toPng cs = (magic `BS.append`) . BS.concat . (<$> cs ++ [Chunk "IEND" ""]) $ \c ->
	be (BS.length $ dat c)
		`BS.append` typ c 
		`BS.append` dat c
		`BS.append` be (calc $ typ c `BS.append` dat c)
	where
	be :: (Bits a, Integral a) => a -> BS.ByteString
	be = beToByteString 4

idat :: [Chunk] -> BS.ByteString
idat = BS.concat . map dat . filter ((== "IDAT") . typ)

type Chunks = [Chunk]

data Chunk = Chunk {
	typ :: BS.ByteString,
	dat :: BS.ByteString
	} deriving (Show, Eq)

chunk :: StateT BS.ByteString Maybe Chunk
chunk = do
	td <- (`item` Just) . (4 +) =<< item 4 (Just . beFromByteString)
	cs <- item 4 $ Just . beFromByteString
	unless (check td cs) $ fail "error"
	return . uncurry Chunk $ BS.splitAt 4 td

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = bool (return []) <$> ((<$> untilM p m) . (:)) <*> not . p =<< m

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)
