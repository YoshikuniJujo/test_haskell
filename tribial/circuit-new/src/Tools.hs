{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Arrow
import Control.Monad.State
import Data.Map (Map)

import qualified Data.Map as M

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

mapAndCollect :: (k -> v1 -> (Maybe a, v2)) -> Map k v1 -> ([a], Map k v2)
mapAndCollect f = M.mapAccumWithKey (\ks -> (first (maybe ks (: ks)) .) . f) []

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)
listToTuple2 _ = error "listToTuple2: Oops!"
