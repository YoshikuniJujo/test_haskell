{-# LANGUAGE PackageImports, TupleSections, OverloadedStrings #-}

module Chunks (Chunks, Chunk(..), fromPng, toPng) where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Bits
import Data.Bool
import Data.Word
import qualified Data.ByteString as BS

import CRC (check, calc)

magic :: BS.ByteString
magic = "\x89PNG\r\n\SUB\n"

fromPng :: BS.ByteString -> Maybe [Chunk]
fromPng = evalStateT $ do
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
	be = ((BS.pack . reverse) .) . flip curry (4 :: Int) . unfoldr $ \(i, n) ->
		bool Nothing (Just . second (i - 1 ,) $ popByte n) (i > 0)

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte = fromIntegral &&& (`shiftR` 8)

type Chunks = [Chunk]

data Chunk = Chunk {
	typ :: BS.ByteString,
	dat :: BS.ByteString
	} deriving (Show, Eq)

chunk :: StateT BS.ByteString Maybe Chunk
chunk = do
	td <- (`item` Just) . (4 +) =<< item 4 (Just . BS.foldl' be 0)
	cs <- item 4 $ Just . BS.foldl' be 0
	unless (check td cs) $ fail "error"
	return . uncurry Chunk $ BS.splitAt 4 td
	where
	be :: (Bits n, Num n) => n -> Word8 -> n
	be = curry $ uncurry (.|.) . ((`shiftL` 8) *** fromIntegral)

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = bool (return []) <$> ((<$> untilM p m) . (:)) <*> not . p =<< m

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)
