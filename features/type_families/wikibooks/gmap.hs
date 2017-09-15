{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import qualified Data.IntMap as IM

class GMapKey k where
	data GMap k :: * -> *
	empty :: GMap k v
	lookup :: k -> GMap k v -> Maybe v
	insert :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
	data GMap Int v = GMapInt (IM.IntMap v)
	empty = GMapInt IM.empty
	lookup k (GMapInt m) = IM.lookup k m
	insert k v (GMapInt m) = GMapInt $ IM.insert k v m
