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
	land :: M.Map (Int, Int) Color
	} deriving Show

moveLeft, moveRight, moveDown :: State -> State
moveLeft s@State { position = (x, y) } = bool s s { position = (x - 1, y) } (x > 0)
moveRight s@State { position = (x, y) } = bool s s { position = (x + 1, y) } (x < 7)
moveDown s@State { position = (x, y) } = bool (landing s) s' . not $ overlap s'
	where s' = s { position = (x, y + 1) }

landing :: State -> State
landing s@State { position = (x, y), land = l } = removeLines (checkLines s') s'
	where s' = s { position = (4, 1), land = insertAllKey [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)] cyan l }

insertAllKey :: Ord k => [k] -> a -> M.Map k a -> M.Map k a
insertAllKey ks v m = foldl (flip $ flip M.insert v) m ks

overlap :: State -> Bool
overlap State { position = (x, y), land = l } =
	any (isJust . flip M.lookup l) [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)] || y > 21

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
