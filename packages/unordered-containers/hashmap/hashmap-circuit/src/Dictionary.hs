{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Dictionary where

import Control.Arrow
import Data.Maybe
import Data.Hashable

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM

class IsDictionary d where
	type Key d
	empty :: d v
	(!?) :: d v -> Key d -> Maybe v
	insert :: Key d -> v -> d v -> d v
	keys :: d v -> [Key d]

	(!) :: d v -> Key d -> v
	elems :: d v -> [v]
	mapWithKey :: (Key d -> v -> v') -> d v -> d v'
	mapAccumWithKey ::
		(a -> Key d -> v -> (a, v')) -> a -> d v -> (a, d v')
	fromList :: [(Key d, v)] -> d v
	toList :: d v -> [(Key d, v)]

	d ! k = fromJust $ d !? k
	elems d = (d !) <$> keys d
	mapWithKey f = snd . mapAccumWithKey (\() k -> (() ,) . f k) ()
	mapAccumWithKey f a0 d = fromList `second` mawk a0 kvs []
		where
		mawk a [] l = (a, l)
		mawk a ((k, v) : kv) l =
			let (a', v') = f a k v in mawk a' kv ((k, v') : l)
		kvs = toList d
	fromList = foldr (uncurry insert) empty
	toList d = (id &&& (d !)) <$> keys d

mapAndCollect :: IsDictionary d =>
	(Key d -> v -> (Maybe a, v')) -> d v -> ([a], d v')
mapAndCollect f = mapAccumWithKey (\ks -> (first (maybe ks (: ks)) .) . f) []

instance Ord k => IsDictionary (M.Map k) where
	type Key (M.Map k) = k
	empty = M.empty
	(!?) = (M.!?)
	insert = M.insert
	keys = M.keys

	(!) = (M.!)
	elems = M.elems
	mapWithKey = M.mapWithKey
	mapAccumWithKey = M.mapAccumWithKey
	fromList = M.fromList
	toList = M.toList

instance (Eq k, Hashable k) => IsDictionary (HM.HashMap k) where
	type Key (HM.HashMap k) = k
	empty = HM.empty
	(!?) = flip HM.lookup
	insert = HM.insert
	keys = HM.keys

	(!) = (HM.!)
	elems = HM.elems
	mapWithKey = HM.mapWithKey
	fromList = HM.fromList
	toList = HM.toList
