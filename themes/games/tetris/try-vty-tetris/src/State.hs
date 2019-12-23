{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State where

import Control.Arrow
import Data.Bool
import Data.Maybe
import Data.List
import Graphics.Vty

import qualified Data.Map as M

data State = State {
	position :: (Int, Int),
	shape :: [(Int, Int)],
	shapeColor :: Color,
	land :: M.Map (Int, Int) Color,
	shapeList :: [([(Int, Int)], Color)],
	score :: Int
	} deriving Show

moveLeft, moveRight, moveDown, rotateLeft :: State -> State
moveLeft s@State { position = (x, y) } = bool s s' $ inside s' && not (overlap s')
	where s' = s { position = (x - 1, y) }
moveRight s@State { position = (x, y) } = bool s s' $ inside s' && not (overlap s')
	where s' = s { position = (x + 1, y) }
moveDown s@State { position = (x, y) } = bool (landing s) s' $ inside s' && not (overlap s')
	where s' = s { position = (x, y + 1) }
rotateLeft s@State { shape = sp } = bool s s' . not $ overlap s'
	where s' = s { shape = rotL <$> sp }
rotateRight s@State { shape = sp } = bool s s' . not $ overlap s'
	where s' = s { shape = rotR <$> sp }

moveBottom :: State -> State
moveBottom s@State { position = (x, y) }
	| inside s' && not (overlap s') = moveBottom s'
	| otherwise = s
	where s' = s { position = (x, y + 1) }

rotL, rotR :: (Int, Int) -> (Int, Int)
rotL (x, y) = (- y, x)
rotR (x, y) = (y, - x)

landing :: State -> State
landing s@State { position = (x, y), shapeColor = c, land = l, shapeList = (sp, c') : sps } =
	removeLines is s' { score = score s + (length is) ^ 2 * 100 }
	where
	is = checkLines s'
	s' = s { position = (4, 1), shape = sp, shapeColor = c', land = insertAllKey (blocks s) c l, shapeList = sps }

insertAllKey :: Ord k => [k] -> a -> M.Map k a -> M.Map k a
insertAllKey ks v m = foldl (flip $ flip M.insert v) m ks

overlap :: State -> Bool
overlap s@State { position = (x, y), land = l } =
	any (isJust . flip M.lookup l) (blocks s)

inside :: State -> Bool
inside = all is . blocks
	where is (x, y) = 0 <= x && x < 10 && 0 <= y && y < 23

blocks :: State -> [(Int, Int)]
blocks State { position = (x, y), shape = s } = (\(dx, dy) -> (x + dx, y + dy)) <$> s

checkLines :: State -> [Int]
checkLines State { land = l } =
	(fst <$>) . filter (all isJust . snd) $ (<$> [0 .. 23]) \y -> (y, (<$> [0 .. 9]) \x -> M.lookup (x, y) l)

removeLines :: [Int] -> State -> State
removeLines is s@State { land = l } = s { land = removeWithRule (removeRule is) l }

removeWithRule :: [(Int, Int)] -> M.Map (Int, Int) v -> M.Map (Int, Int) v
removeWithRule r m = moveMap krs m
	where
	krs = (\(o, n) -> (\x -> ((x, o), (x, n))) <$> [0 .. 9]) =<< r

moveMap :: Ord k => [(k, k)] -> M.Map k v -> M.Map k v
moveMap [] _ = M.empty
moveMap ((k, k') : r) m = maybe (moveMap r m) (\v -> M.insert k' v $ moveMap r m) $ M.lookup k m


removeRule :: [Int] -> [(Int, Int)]
removeRule is = uncurry zip $ ((\\ is) &&& id) [23, 22 .. 0]
