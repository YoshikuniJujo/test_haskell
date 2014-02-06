{-# LANGUAGE TupleSections #-}

module AI (aiN) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (***))
import Data.List (partition)

import Game (
	Game, Turn(..), X(..), Y(..), Disk(..),
	turn, disks, placeable, initGame, nextGame)
import Tools (flipEnum, forMaybe, maximumBySnd)

type Table = [((X, Y), Int)]

flipXY, flipX, flipY :: (X, Y) -> (X, Y)
flipXY (x, y) = (toEnum $ fromEnum y, toEnum $ fromEnum x)
flipX = first flipEnum
flipY = second flipEnum

score :: Table -> (X, Y) -> Int
score t pos@(x, y)
	| x > D = score t $ flipX pos
	| y > Y4 = score t $ flipY pos
	| fromEnum x < fromEnum y = score t $ flipXY pos
score t pos = case lookup pos t of
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
	((A, Y1),  30),
	((B, Y1), -12),
	((B, Y2), -15),
	((C, Y1),   0),
	((C, Y2),  -3),
	((C, Y3),   0),
	((D, Y1),  -1),
	((D, Y2),  -3),
	((D, Y3),  -1),
	((D, Y4),  -1) ]

divide :: Disk -> Game -> ([(X, Y)], [(X, Y)])
divide d g = map fst *** map fst $ partition ((== d) . snd) $ disks g

phase :: Game -> Int
phase = length . disks

evaluateWith :: (Int -> (X, Y) -> Int) -> Disk -> Game -> Int
evaluateWith scr d g = ss me - ss you
	where
	ss = sum . map (scr $ phase g)
	(me, you) = divide d g

evaluate :: Disk -> Game -> Int
evaluate = evaluateWith $ \p -> score $ if p < 32 then table1 else table2

evaluateResult :: Disk -> Game -> Int
evaluateResult = evaluateWith $ \_ _ -> 1000

nextGames :: Game -> [((X, Y), Game)]
nextGames g = forMaybe (placeable g) $ \pos -> (pos ,) <$> nextGame g pos

ai0 :: Game -> Maybe ((X, Y), Int)
ai0 g = case turn g of
	Turn d -> Just $ maximumBySnd $ map (second $ evaluate d) $ nextGames g
	_ -> Nothing

aiN :: Int -> Game -> Maybe ((X, Y), Int)
aiN 0 g = ai0 g
aiN n g = case turn g of
	Turn d -> Just $ maximumBySnd $ forMaybe (nextGames g) $
		\(pos, ng) -> (pos ,) <$> case turn ng of
			Turn nd -> (if d == nd then id else negate) .
				snd <$> aiN (n - 1) ng
			_ -> Just $ evaluateResult d ng
	_ -> Nothing
