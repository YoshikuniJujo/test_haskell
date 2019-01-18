module DictMap where

import Control.Arrow
import Data.Bool

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)

type Dict k v = Map k v

fromListDict :: Ord k => [(k, v)] -> Dict k v
fromListDict = M.fromList

emptyDict :: Dict k v
emptyDict = M.empty

lookupDict :: Ord k => k -> Dict k v -> Maybe v
lookupDict = M.lookup

keysDict :: Dict k v -> [k]
keysDict = M.keys

elemsDict :: Dict k v -> [v]
elemsDict = M.elems

insertDict :: Ord k => k -> v -> Dict k v -> Dict k v
insertDict = M.insert

updateAndAccum :: (k -> (v, Maybe a)) -> [a] -> Dict k v -> ([a], Dict k v)
updateAndAccum = mapAccumWithKeyDict . fun

mapAccumWithKeyDict ::
	(a -> k -> v -> (a, v')) -> a -> Dict k v -> (a, Dict k v')
mapAccumWithKeyDict = M.mapAccumWithKey

nw :: Int -> (Double, Maybe String)
nw n = (fromIntegral n, bool Nothing (Just $ show n) $ even n)

fun :: (k -> (v, Maybe a)) -> [a] -> k -> v -> ([a], v)
fun f xs k _ = let (v, mx) = f k in (($ xs) $ maybe id (:) mx, v)

restrictKeysDict :: Ord k => Dict k v -> Set k -> Dict k v
restrictKeysDict = M.restrictKeys
