{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

push :: Ord k => k -> v -> Map k [v] -> Map k [v]
push k v m = let vs = v : fromMaybe [] (m !? k) in insert k vs m

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "Oops!"
