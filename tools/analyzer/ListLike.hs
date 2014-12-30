{-# LANGUAGE TypeFamilies #-}

module ListLike (ListLike(..)) where

import Prelude hiding (splitAt, span)
import qualified Prelude as P

import Data.List
import Data.Word8

import qualified Data.ByteString as BS

class ListLike a where
	type Element a
	uncons :: a -> Maybe (Element a, a)
	splitAt :: Integer -> a -> (a, a)
	span :: (Element a -> Bool) -> a -> (a, a)

instance ListLike [a] where
	type Element [a] = a
	uncons (x : xs) = Just (x, xs)
	uncons _ = Nothing
	splitAt = genericSplitAt
	span = P.span

instance ListLike BS.ByteString where
	type Element BS.ByteString = Word8
	uncons = BS.uncons
	splitAt = BS.splitAt . fromIntegral
	span = BS.span
