{-# Language TupleSections #-}

module AI (aiN) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (***))
import Control.Monad (forM)
import Data.List (partition, maximumBy)
import Data.Function (on)

import Game
import Tools

ai0 :: Game -> Maybe ((X, Y), Int)
ai0 g	| null $ putable g = Nothing
	| otherwise = fmap (maximumBy (on compare snd)) $ forM (putable g) $
		\pos -> do
			ng <- nextGame g pos
			return (pos, calc ng $ stone $ turn g)

aiN :: Int -> Game -> Maybe ((X, Y), Int)
aiN 0 g = ai0 g
aiN n g = do
	allPair <- mapM (\pos -> (pos ,) <$> nextGame g pos) $ putable g
	rets <- flip mapM allPair $ \(pos, ng) -> case turn ng of
		GameOver -> return (pos, calcR g (stone $ turn g))
		_ -> (const pos *** negate) <$> aiN (n - 1) ng
	return $ maximumBy (on compare snd) rets

----------------------------------------------------------------------
-- calc :: Game -> Stone -> Int

calc, calcR :: Game -> Stone -> Int
calc = sumPoint gp
	where
	gp t	| t < 32 = getPoint map1
		| otherwise = getPoint map2
calcR = sumPoint (const2 1)

sumPoint :: (Int -> (X, Y) -> Int) -> Game -> Stone -> Int
sumPoint gp g s = sp me - sp you
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
