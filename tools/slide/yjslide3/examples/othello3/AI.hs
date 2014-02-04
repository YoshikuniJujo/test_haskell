{-# Language TupleSections #-}

module AI (aiN) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.List (partition)
import Game (
	Disk(..), Game, Turn(..), X(..), Y(..),
	nextGame, turn, disks, placeable)
import Tools (maximumBySnd, flipEnum, forMaybe)

ai0 :: Game -> Maybe ((X, Y), Int)
ai0 g = case turn g of
	Turn d -> Just $ maximumBySnd $ map (second $ calc d) $ nextGames g
	_ -> Nothing

aiN :: Int -> Game -> Maybe ((X, Y), Int)
aiN 0 g = ai0 g
aiN n g = case turn g of
	Turn d -> Just $ maximumBySnd $ forMaybe (nextGames g) $ \(p, ng) ->
		fmap (p ,) $ case turn ng of
			Turn nd -> (if d == nd then id else negate) .
				snd <$> aiN (n - 1) ng
			_ -> Just $ calcResult d ng
	_ -> Nothing

nextGames :: Game -> [((X, Y), Game)]
nextGames g = forMaybe (placeable g) $ \pos -> (pos ,) <$> nextGame g pos

----------------------------------------------------------------------
-- calc, calcResult :: Disk -> Game -> Int

calc, calcResult :: Disk -> Game -> Int
calc = calcWith $ \t -> point $ if t < 32 then table1 else table2
calcResult = calcWith (\_ _ -> 1000)

calcWith :: (Int -> (X, Y) -> Int) -> Disk -> Game -> Int
calcWith pnt d g = sp me - sp you
	where
	sp = sum . map (pnt (length ds) . fst)
	(me, you) = partition ((== d) . snd) ds
	ds = disks g

type Table = [((X, Y), Int)]

flipXY, flipX, flipY :: (X, Y) -> (X, Y)
flipXY (x, y) = (toEnum $ fromEnum y, toEnum $ fromEnum x)
flipX = first flipEnum
flipY = second flipEnum

point :: Table -> (X, Y) -> Int
point t pos@(x, y)
	| x > D = point t $ flipX pos
	| y > Y4 = point t $ flipY pos
	| fromEnum x < fromEnum y = point t $ flipXY pos
point t pos = case lookup pos t of
	Just p -> p
	_ -> error "bad table"

table2 :: Table
table2 = [
	((A, Y1), 120),
	((B, Y1), -20),
	((B, Y2), -40),
	((C, Y1), 20),
	((C, Y2), -5),
	((C, Y3), 15),
	((D, Y1), 5),
	((D, Y2), -5),
	((D, Y3), 3),
	((D, Y4), 3) ]

table1 :: Table
table1 = [
	((A, Y1), 30),
	((B, Y1), -12),
	((B, Y2), -15),
	((C, Y1), 0),
	((C, Y2), -3),
	((C, Y3), 0),
	((D, Y1), -1),
	((D, Y2), -3),
	((D, Y3), -1),
	((D, Y4), -1) ]
