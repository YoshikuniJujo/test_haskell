{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State where

import Data.Bool
import Data.Maybe
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
landing s@State { position = (x, y), land = l } =
	s { position = (4, 1), land = insertAllKey [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)] cyan l }

insertAllKey :: Ord k => [k] -> a -> M.Map k a -> M.Map k a
insertAllKey ks v m = foldl (flip $ flip M.insert v) m ks

overlap :: State -> Bool
overlap State { position = (x, y), land = l } =
	any (isJust . flip M.lookup l) [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)] || y > 21
