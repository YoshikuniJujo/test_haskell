{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import Data.Vector.Unboxed
import Data.Vector.Unboxed.Mutable

import qualified Control.Monad.State as State
import qualified Data.Vector.Unboxed as V

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* State.modify m

mapAndUpdate :: (Unbox a, Unbox v, Unbox v') =>
	(Int -> v -> (Maybe (Int, a), v')) -> Vector a -> Vector v -> (Vector a, Vector v')
mapAndUpdate f upd vec = runST $ do
	upd' <- thaw upd
	vec' <- new $ V.length vec
	iref <- newSTRef 0
	V.forM_ vec $ \v -> do
		i <- readSTRef iref
		let	(ma, v') = f i v
		maybe (return ()) (uncurry $ write upd') ma
		write vec' i v'
		writeSTRef iref $ i + 1
	(,) <$> freeze upd' <*> freeze vec'

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "listToTuple3: error"
