{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple (

	T, empty, update, indexLength, distance

	) where

import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Map qualified as Map
import Data.Sequence qualified as Seq	
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

-- * API

type T = (Int, Seq.Seq Word8, Map.Map BS.ByteString [Int])

update :: T -> Word8 -> T
update = push

indexLength :: Monad m => T -> Word8 -> Word8 -> Word8 -> m (Maybe Word8) -> m (Maybe (Int, Int))
indexLength t b0 b1 b2 = maxLengthFromTriple t $ BS.pack [b0, b1, b2]

empty :: T
empty = (0, Seq.empty, Map.empty)

distance :: Int -> T -> Int
distance i' (i, s, _) = i + Seq.length s - i' + 3

-- * INTERNAL

maxOffset :: Int
maxOffset = 32768

push :: T -> Word8 -> T
push = pushWithMax maxOffset

pushWithMax :: Int -> T -> Word8 -> T
pushWithMax mx (i, s, d) b = (i', s' Seq.|> b, d')
	where
	ln = Seq.length s
	i' = bool i (i + 1) (ln >= mx)
	s' = bool s (tailS s) (ln >= mx)
	j = i + ln
	tr = BS.pack $ toList (takeR 2 s) ++ [b]
	d' = Map.alter (modifyElem i' j) tr d

modifyElem :: Int -> Int -> Maybe [Int] -> Maybe [Int]
modifyElem i j = \case
	Nothing -> Just [j + 1]
	Just is -> Just $ (j + 1) : takeWhile (>= i) is

takeR :: Int -> Seq.Seq a -> Seq.Seq a
takeR n s = Seq.drop (Seq.length s - n) s

tailS :: Seq.Seq a -> Seq.Seq a
tailS s = case Seq.viewl s of
	(_ Seq.:< s') -> s'
	_ -> error "bad"

maxLengthFromTriple :: Monad m => T -> BS.ByteString -> m (Maybe Word8) -> m (Maybe (Int, Int))
maxLengthFromTriple st tr gb = maxLength (getRotables st tr) gb

getIndices :: T -> BS.ByteString -> [Int]
getIndices (_, _, d) tr = fromMaybe [] $ Map.lookup tr d

indexToRotate :: T -> BS.ByteString -> Int -> (Int, Rotable Word8)
indexToRotate (i, s, _) tr i' = (i', (s', s'))
	where s' = Seq.drop (i' - i) s Seq.>< Seq.fromList (BS.unpack tr)

getRotables :: T -> BS.ByteString -> [(Int, Rotable Word8)]
getRotables st tr = indexToRotate st tr <$> is
	where
	is = getIndices st tr

maxLength :: (Monad m, Eq a) => [(Int, Rotable a)] -> m (Maybe a) -> m (Maybe (Int, Int))
maxLength [] _ = pure Nothing
maxLength rs gb = gb >>= \case
	Nothing -> pure $ Just (fst $ head rs, 0)
	Just b -> do
		case checkStrings rs b of
			[] -> pure $ Just (fst $ head rs, 0)
			rs' -> (((+ 1) `second`) <$>) <$> maxLength rs' gb

checkStrings :: Eq a => [(Int, Rotable a)] -> a -> [(Int, Rotable a)]
checkStrings rs b = (`check1` b) `mapMaybe` rs

check1 :: Eq a => (Int, Rotable a) -> a -> Maybe (Int, Rotable a)
check1 r b
	| b == b' = Just (fst r, r')
	| otherwise = Nothing
	where
	(b', r') = unconsR $ snd r

type Rotable a = (Seq.Seq a, Seq.Seq a)

unconsR :: Rotable a -> (a, Rotable a)
unconsR (s, p) = case Seq.viewl p of
	Seq.EmptyL -> unconsR (s, s)
	x Seq.:< p' -> (x, (s, p'))
