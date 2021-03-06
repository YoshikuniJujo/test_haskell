{-# LANGUAGE OverloadedStrings, PackageImports #-}

module ReadIdat (idat, item) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad (unless)
import "monads-tf" Control.Monad.State (StateT, evalStateT, put, gets)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Bool (bool)
import Data.Word (Word8)
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
	td <- (`item` Just) . (4 +) =<< item 4 (Just . BS.foldl' be 0)
	cs <- item 4 $ Just . BS.foldl' be 0
	unless (check td cs) $ fail "error"
	return $ BS.splitAt 4 td
	where
	be :: (Bits n, Num n) => n -> Word8 -> n
	be = curry $ uncurry (.|.) . ((`shiftL` 8) *** fromIntegral)

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = bool (return []) <$> ((<$> untilM p m) . (:)) <*> not . p =<< m

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)
