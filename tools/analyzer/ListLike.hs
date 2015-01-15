{-# LANGUAGE TypeFamilies #-}

module ListLike (ListLike(..), null, span) where

import Prelude hiding (splitAt, null, span)

import Data.Maybe (isNothing)
import Data.Word8 (Word8)

import qualified Data.ByteString as BS

class ListLike a where
	type Element a
	empty :: a
	cons :: Element a -> a -> a
	uncons :: a -> Maybe (Element a, a)
	splitAt :: Integer -> a -> (a, a)
	splitAt n xs | n <= 0 = (empty, xs) | otherwise =
		case uncons xs of
			Just (h, t) -> let
				(u, v) = splitAt (n - 1) t
				in (cons h u, v)
			_ -> (empty, empty)

instance ListLike [a] where
	type Element [a] = a
	empty = []
	cons = (:)
	uncons (x : xs) = Just (x, xs)
	uncons _ = Nothing

instance ListLike BS.ByteString where
	type Element BS.ByteString = Word8
	empty = BS.empty
	cons = BS.cons
	uncons = BS.uncons
	splitAt = BS.splitAt . fromIntegral

null :: ListLike a => a -> Bool
null = isNothing . uncons

span :: ListLike a => (Element a -> Bool) -> a -> (a, a)
span p s = case uncons s of
	Just (h, t) | p h ->
		let (u, v) = span p t in (cons h u, v)
	_ -> (empty, s)
