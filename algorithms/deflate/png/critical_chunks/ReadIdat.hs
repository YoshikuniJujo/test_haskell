{-# LANGUAGE OverloadedStrings, PackageImports #-}

module ReadIdat (idat) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad (when)
import "monads-tf" Control.Monad.State (StateT, evalStateT, put, gets)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Bool (bool)
import qualified Data.ByteString as BS

import CRC (check)

magic :: BS.ByteString
magic = "\x89PNG\r\n\SUB\n"

type Chunk = (BS.ByteString, BS.ByteString)

iend :: Chunk
iend = ("IEND", "")

idat :: BS.ByteString -> Maybe BS.ByteString
idat = ((BS.concat . map snd . filter ((== "IDAT") . fst) <$>) .) . evalStateT $
	item 8 (bool Nothing (Just ()) . (== magic)) >> untilM (== iend) chunk

chunk :: StateT BS.ByteString Maybe Chunk
chunk = do
	td <- (`item` Just) . (4 +) =<< item 4 (Just . be)
	cs <- item 4 $ Just . be
	when (not $ check td cs) $ fail "error"
	return $ BS.splitAt 4 td
	where
	be :: (Bits n, Num n) => BS.ByteString -> n
	be = BS.foldl' (\n w -> n `shiftL` 8 .|. fromIntegral w) 0

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = bool (return []) <$> ((<$> untilM p m) . (:)) <*> not . p =<< m

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)
