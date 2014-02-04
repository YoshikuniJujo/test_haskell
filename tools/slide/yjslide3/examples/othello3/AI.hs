{-# Language TupleSections #-}

module AI (aiN) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.List (partition)

import Game
import Tools

ai0 :: Game -> Maybe ((X, Y), Int)
ai0 g = case turn g of
	Turn s -> Just $ maximumBySnd $ map (second $ calc s) $ nextGames g
	_ -> Nothing

aiN :: Int -> Game -> Maybe ((X, Y), Int)
aiN 0 g = ai0 g
aiN n g = case turn g of
	Turn s -> Just $ maximumBySnd $ forMaybe (nextGames g) $ \(pos, ng) ->
		fmap (pos ,) $ case turn ng of
			Turn ns -> (if s == ns then id else negate) .
				snd <$> aiN (n - 1) ng
			_ -> Just $ calcR s ng
	_ -> Nothing

nextGames :: Game -> [((X, Y), Game)]
nextGames g = forMaybe (putable g) $ \pos -> (pos ,) <$> nextGame g pos

----------------------------------------------------------------------
-- calc, calcR :: Stone -> Game -> Int

calc, calcR :: Stone -> Game -> Int
calc = sumPoint $ \t -> getPoint $ if t < 32 then map1 else map2
calcR = sumPoint (const2 1)

sumPoint :: (Int -> (X, Y) -> Int) -> Stone -> Game -> Int
sumPoint gp s g = sp me - sp you
	where
	sp = sum . map (gp t . fst)
	t = length $ stones g
	(me, you) = partition ((== s) . snd) $ stones g

type Map = [((X, Y), Int)]

flipXY, flipX, flipY :: (X, Y) -> (X, Y)
flipXY (x, y) = (toEnum $ fromEnum y, toEnum $ fromEnum x)
flipX = first flipE
flipY = second flipE

getPoint :: Map -> (X, Y) -> Int
getPoint m pos@(x, y)
	| x > D = getPoint m $ flipX pos
	| y > Y4 = getPoint m $ flipY pos
	| fromEnum x < fromEnum y = getPoint m $ flipXY pos
getPoint m pos = case lookup pos m of
	Just p -> p
	_ -> error "bad map"

map2 :: Map
map2 = [
	((A, Y1), 120),
	((B, Y1), -20),
	((B, Y2), -40),
	((C, Y1), 20),
	((C, Y2), -5),
	((C, Y3), 15),
	((D, Y1), 5),
	((D, Y2), -5),
	((D, Y3), 3),
	((D, Y4), 3)
 ]

map1 :: Map
map1 = [
	((A, Y1), 30),
	((B, Y1), -12),
	((B, Y2), -15),
	((C, Y1), 0),
	((C, Y2), -3),
	((C, Y3), 0),
	((D, Y1), -1),
	((D, Y2), -3),
	((D, Y3), -1),
	((D, Y4), -1)
 ]
