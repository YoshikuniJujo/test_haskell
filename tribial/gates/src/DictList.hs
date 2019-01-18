module DictList where

import Control.Arrow
import Data.Bool

type Dict k v = [(k, v)]

fromListDict :: [(k, v)] -> Dict k v
fromListDict = id

emptyDict :: Dict k v
emptyDict = []

lookupDict :: Eq k => k -> Dict k v -> Maybe v
lookupDict = lookup

keysDict :: Dict k v -> [k]
keysDict = map fst

elemsDict :: Dict k v -> [v]
elemsDict = map snd

insertDict :: k -> v -> Dict k v -> Dict k v
insertDict k v = ((k, v) :)

updateAndAccum :: (k -> (v, Maybe a)) -> [a] -> Dict k v -> ([a], Dict k v)
updateAndAccum = mapAccumWithKeyDict . fun

mapAccumWithKeyDict ::
	(a -> k -> v -> (a, v')) -> a -> Dict k v -> (a, Dict k v')
mapAccumWithKeyDict _ x [] = (x, [])
mapAccumWithKeyDict f x ((k, v) : d) = let
	(x', v') = f x k v in
	second ((k, v') :) $ mapAccumWithKeyDict f x' d

nw :: Int -> (Double, Maybe String)
nw n = (fromIntegral n, bool Nothing (Just $ show n) $ even n)

fun :: (k -> (v, Maybe a)) -> [a] -> k -> v -> ([a], v)
fun f xs k _ = let (v, mx) = f k in (($ xs) $ maybe id (:) mx, v)
