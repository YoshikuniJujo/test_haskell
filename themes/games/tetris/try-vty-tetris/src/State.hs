{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State (
	State, blocks, moveBottom, shapeList, land, shapeColor, position, score, drawed,
	moveLeft, moveRight, rotateLeft, rotateRight, pauseGame, pause, pending, initialState, moveDown
	) where

import Control.Arrow
import Data.Bool
import Data.Maybe
import Data.List
import Graphics.Vty

import qualified Data.Map as M

import Minos

data State = State {
	position :: (Int, Int),
	shape :: Mino,
	shapeColor :: Color,
	land :: M.Map (Int, Int) Color,
	shapeList :: [(Mino, Color)],
	score :: Int,
	pause :: Bool,
	blockNumber :: Int,
	moveDownCounter :: Int,
	pending :: Bool
	} deriving Show

initialState :: [(Mino, Color)] -> State
initialState (sp0 : sps) = State (4, 1) (fst sp0) (snd sp0) M.empty sps 0 False 0 0 True
initialState [] = error "empty minos not allowed"

drawed :: State -> State
drawed s = s { pending = False }

moveLeft, moveRight, moveDown, rotateLeft :: State -> State
moveLeft s@State { position = (x, y) } = bool s s' $ inside s' && not (overlap s')
	where s' = s { position = (x - 1, y), pending = True }
moveRight s@State { position = (x, y) } = bool s s' $ inside s' && not (overlap s')
	where s' = s { position = (x + 1, y), pending = True }

moveDown s@State { moveDownCounter = c, blockNumber = bn } | c < (if n < 4 then 4 else n) = s { moveDownCounter = c + 1 }
	where n = 10 - bn `div` 10
moveDown s@State { position = (x, y) } = bool (landing s) s' $ inside s' && not (overlap s')
	where s' = s { position = (x, y + 1), moveDownCounter = 0, pending = True }

rotateLeft s@State { shape = sp } = tryStates s (shapeCandidates s $ rotateMinoL sp)
rotateRight s@State { shape = sp } = tryStates s (shapeCandidates s $ rotateMinoR sp)

shapeCandidates :: State -> [Mino] -> [State]
shapeCandidates s = ((\sp -> s { shape = sp }) <$>)

okState :: State -> Bool
okState s = inside s && not (overlap s)

tryStates :: State -> [State] -> State
tryStates s ss = case dropWhile (not . okState) ss of [] -> s; s' : _ -> s' { pending = True }

moveBottom :: State -> State
moveBottom s@State { position = (x, y) }
	| inside s' && not (overlap s') = moveBottom s'
	| otherwise = s
	where s' = s { position = (x, y + 1), pending = True }

rotL, rotR :: (Int, Int) -> (Int, Int)
rotL (x, y) = (- y, x)
rotR (x, y) = (y, - x)

landing :: State -> State
landing s@State { position = (x, y), shapeColor = c, land = l, shapeList = (sp, c') : sps } =
	removeLines is s' { score = score s + (length is) ^ 2 * 100, pending = True }
	where
	is = checkLines s'
	s' = s { position = (4, 1), shape = sp, shapeColor = c', land = insertAllKey (blocks s) c l, shapeList = sps, blockNumber = blockNumber s + 1 }

insertAllKey :: Ord k => [k] -> a -> M.Map k a -> M.Map k a
insertAllKey ks v m = foldl (flip $ flip M.insert v) m ks

overlap :: State -> Bool
overlap s@State { position = (x, y), land = l } =
	any (isJust . flip M.lookup l) (blocks s)

inside :: State -> Bool
inside = all is . blocks
	where is (x, y) = 0 <= x && x < 10 && 0 <= y && y < 23

blocks :: State -> [(Int, Int)]
blocks State { position = (x, y), shape = s } = minoToPos s (x, y) -- (\(dx, dy) -> (x + dx, y + dy)) <$> s

checkLines :: State -> [Int]
checkLines State { land = l } =
	(fst <$>) . filter (all isJust . snd) $ (<$> [0 .. 23]) \y -> (y, (<$> [0 .. 9]) \x -> M.lookup (x, y) l)

removeLines :: [Int] -> State -> State
removeLines is s@State { land = l } = s { land = removeWithRule (removeRule is) l, pending = True }

removeWithRule :: [(Int, Int)] -> M.Map (Int, Int) v -> M.Map (Int, Int) v
removeWithRule r m = moveMap krs m
	where
	krs = (\(o, n) -> (\x -> ((x, o), (x, n))) <$> [0 .. 9]) =<< r

moveMap :: Ord k => [(k, k)] -> M.Map k v -> M.Map k v
moveMap [] _ = M.empty
moveMap ((k, k') : r) m = maybe (moveMap r m) (\v -> M.insert k' v $ moveMap r m) $ M.lookup k m


removeRule :: [Int] -> [(Int, Int)]
removeRule is = uncurry zip $ ((\\ is) &&& id) [23, 22 .. 0]

pauseGame :: State -> State
pauseGame s = s { pause = not $ pause s }
