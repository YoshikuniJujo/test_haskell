{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Pipe.Runlength.Triple (

	-- * TYPE

	T, empty,

	-- * UPDATE

	update,

	-- * QUERY

	indexLength', indexLength, distance

	) where

import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Map qualified as Map
import Data.Sequence qualified as Seq	
import Data.Bool
import Data.Word

-- TYPE

type T = (Int, Seq.Seq Word8, Map.Map [Word8] [Int])

empty :: T
empty = (0, Seq.empty, Map.empty)

-- UPDATE

update :: T -> Word8 -> T
update = push maxOffset

maxOffset :: Int
maxOffset = 32768

push :: Int -> T -> Word8 -> T
push mx (i, s, d) b = (i', s' Seq.|> b, d')
	where
	(i', s') = bool (i, s) (i + 1, tailS s) (ln >= mx)
	d' = Map.alter (add $ i + ln + 1) tr d
	add j = maybe (Just [j]) (Just . (j :) . takeWhile (>= i'))
	ln = Seq.length s
	tr = toList (takeR 2 s) ++ [b]

tailS :: Seq.Seq a -> Seq.Seq a
tailS s = case Seq.viewl s of (_ Seq.:< s') -> s'; _ -> error "bad"

takeR :: Int -> Seq.Seq a -> Seq.Seq a
takeR n s = Seq.drop (Seq.length s - n) s

-- QUERY

indexLength' :: Monad m =>
	T -> Word8 -> Word8 -> Word8 -> m (Maybe Word8) -> m (Maybe (Int, Int))
indexLength' t b0 b1 b2 gb = (((255 `min`) `second`) <$>) <$> indexLength t b0 b1 b2 gb

indexLength :: Monad m =>
	T -> Word8 -> Word8 -> Word8 -> m (Maybe Word8) -> m (Maybe (Int, Int))
indexLength t b0 b1 b2 = maxLength $ getRts t [b0, b1, b2]

maxLength :: (Monad m, Eq a) =>
	[(Int, Rotable a)] -> m (Maybe a) -> m (Maybe (Int, Int))
maxLength [] _ = pure Nothing
maxLength rs gb = gb >>= \case
	Nothing -> pure $ Just (fst $ head rs, 0)
	Just b -> case (`chk1` b) `mapMaybe` rs of
		[] -> pure $ Just (fst $ head rs, 0)
		rs' -> (((+ 1) `second`) <$>) <$> maxLength rs' gb

chk1 :: Eq a => (Int, Rotable a) -> a -> Maybe (Int, Rotable a)
chk1 (i, r) b = flip (bool Nothing) `uncurry` ((b ==) *** Just . (i ,) $ uncR r)

getRts :: T -> [Word8] -> [(Int, Rotable Word8)]
getRts (i, s, d) tr = idxToRt i s tr <$> fromMaybe [] (Map.lookup tr d)

idxToRt :: Int -> Seq.Seq Word8 -> [Word8] -> Int -> (Int, Rotable Word8)
idxToRt i s tr i' = (i', (s', s'))
	where s' = Seq.drop (i' - i) s Seq.>< Seq.fromList tr

distance :: Int -> T -> Int
distance i' (i, s, _) = i + Seq.length s - i' + 3

-- ROTABLE

type Rotable a = (Seq.Seq a, Seq.Seq a)

uncR :: Rotable a -> (a, Rotable a)
uncR (s, p) = case Seq.viewl p of
	Seq.EmptyL -> uncR (s, s); x Seq.:< p' -> (x, (s, p'))
