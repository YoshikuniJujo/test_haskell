{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Triple where

import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Map qualified as Map
import Data.Sequence qualified as Seq	
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

-- * API

updateTriple :: Triple -> Word8 -> Triple
updateTriple = push

getIndexLength :: Monad m => Triple -> BS.ByteString -> m Word8 -> m (Maybe (Int, Int))
getIndexLength = maxLengthFromTriple

type Triple = (Int, Seq.Seq Word8, Map.Map BS.ByteString [Int])

triple0 :: Triple
triple0 = (0, Seq.empty, Map.empty)

calcDistance :: Int -> Triple -> Int
calcDistance i' (i, s, _) = i + Seq.length s - i' + 3

-- * INTERNAL

maxOffset :: Int
maxOffset = 32768

push :: Triple -> Word8 -> Triple
push = pushWithMax maxOffset

pushWithMax :: Int -> Triple -> Word8 -> Triple
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
tailS s = let (_ Seq.:< s') = Seq.viewl s in s'

pushByteString :: Int -> Triple -> BS.ByteString -> Triple
pushByteString mx tr = foldl (pushWithMax mx) tr . BS.unpack

maxLengthFromTriple :: Monad m => Triple -> BS.ByteString -> m Word8 -> m (Maybe (Int, Int))
maxLengthFromTriple st tr gb = maxLength (getRotables st tr) gb

getIndices :: Triple -> BS.ByteString -> [Int]
getIndices (_, _, d) tr = fromMaybe [] $ Map.lookup tr d

indexToRotate :: Triple -> BS.ByteString -> Int -> (Int, Rotable Word8)
indexToRotate (i, s, _) tr i' = (i', (s', s'))
	where s' = Seq.drop (i' - i) s Seq.>< Seq.fromList (BS.unpack tr)

getRotables :: Triple -> BS.ByteString -> [(Int, Rotable Word8)]
getRotables st tr = indexToRotate st tr <$> is
	where
	is = getIndices st tr

maxLength :: (Monad m, Eq a) => [(Int, Rotable a)] -> m a -> m (Maybe (Int, Int))
maxLength [] _ = pure Nothing
maxLength rs gb = do
	b <- gb
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
