{-# LANGUAGE OverloadedStrings, PackageImports #-}

module ReadIdat (idat) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import "monads-tf" Control.Monad.State (runState, put, get)
import Data.List (unfoldr)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import CRC (check)

idat :: BS.ByteString -> Maybe BS.ByteString
idat = (BS.concat . map snd . filter ((== "IDAT") . fst) <$>)
	. (sequence . unfoldr chunk =<<) . signature

signature :: BS.ByteString -> Maybe BS.ByteString
signature = uncurry (flip $ bool Nothing) . ((== sig) *** Just) . BS.splitAt 8
	where sig = "\x89PNG\r\n\SUB\n"

type Chunk = (BS.ByteString, BS.ByteString)

chunk :: BS.ByteString -> Maybe (Maybe Chunk, BS.ByteString)
chunk "" = Nothing
chunk bs
	| check (LBS.fromStrict td) c = Just (Just std, r)
	| otherwise = Just (Nothing, r)
	where
	((td, std, c), r) = (`runState` bs) $ do
		l <- st $ BS.splitAt 4
		td_ <- st . BS.splitAt $ 4 + be l
		c_ <- st $ BS.splitAt 4
		return (td_, BS.splitAt 4 td_, be c_)
	st = (uncurry (=<<) . (const . return *** put) =<<) . (<$> get)
	be :: (Bits n, Num n) => BS.ByteString -> n
	be = BS.foldl' (\n w -> n `shiftL` 8 .|. fromIntegral w) 0
