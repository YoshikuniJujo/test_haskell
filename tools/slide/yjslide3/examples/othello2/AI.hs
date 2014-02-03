{-# Language TupleSections #-}

module AI (
	ai0,
	aiN
) where

import Control.Applicative ((<$>))
import Control.Arrow(first, second)

import Data.List (partition, maximumBy)
import Data.Function (on)

import Game

testAI :: Maybe Game -> AI -> Maybe Game
testAI (Just g) ai = do
	(pos, _) <- ai g
	nextGame g pos

type AI = Game -> Maybe ((X, Y), Int)

ai0 :: Game -> Maybe ((X, Y), Int)
ai0 g	| null $ putable g = Nothing
	| otherwise = Just $ maximumBy (on compare snd) $
		map (\pos -> (pos, calc g $ stone $ turn g)) $ putable g

aiN :: Int -> Game -> Maybe ((X, Y), Int)
aiN 0 g = ai0 g
aiN n g = do
	allNext <- mapM (nextGame g) $ putable g
--	rets <- map (second negate) <$> mapM (aiN (n - 1)) allNext
	rets <- do
		allPair <- mapM (\pos -> (pos ,) <$> nextGame g pos) $ putable g
		p <- flip mapM allPair $ \(pos, ng) -> case turn ng of
			GameOver -> return (pos, (undefined, calc g (stone $ turn g)))
			_ -> (pos ,) <$> aiN (n - 1) ng
		return $ map (\(pos, (_, pnt)) -> (pos, negate pnt)) p
	return $ maximumBy (on compare snd) rets

calc :: Game -> Stone -> Int
calc g s
	| t < 32 =  sum (map (getPoint map1 . fst) m) -
			sum (map (getPoint map1 . fst) y)
	| otherwise = sum (map (getPoint map2 . fst) m) -
			sum (map (getPoint map2 . fst) y)
	where
	t = length $ stones g
	(m, y) = partition ((== s) . snd) $ stones g

type Map = [((X, Y), Int)]

flipXY, flipX, flipY :: (X, Y) -> (X, Y)
flipXY (x, y) = (toEnum $ fromEnum y, toEnum $ fromEnum x)
flipX (x, y) = (toEnum $ fromEnum (maxBound :: X) - fromEnum x, y)
flipY (x, y) = (x, toEnum $ fromEnum (maxBound :: Y) - fromEnum y)

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
