{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict
import Data.Word

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

push :: Ord k => k -> v -> Map k [v] -> Map k [v]
push k v m = let vs = v : fromMaybe [] (m !? k) in insert k vs m

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)
listToTuple2 _ = error "Oops!"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "Oops!"

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [x, y, z, w] = (x, y, z, w)
listToTuple4 _ = error "Oops!"

log2 :: (Integral n, Integral m) => n -> m
log2 i = i2 0
	where i2 j
		| j < 0 = error "Oops!"
		| 2 ^ j >= i = j
		| otherwise = i2 $ j + 1

binary :: (a, a) -> Word16 -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)
