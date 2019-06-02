{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IntMapToVector where

import Control.Monad.ST
import Data.Foldable
import Data.Vector.Unboxed
import Data.Vector.Unboxed.Mutable

import qualified Data.IntMap.Strict as IM

intMapToVector :: Unbox v' => (v -> v') -> Int -> IM.IntMap v -> Vector v'
intMapToVector vconv sz m = runST $ do
	vec <- new sz
	for_ (IM.toList m) $ \(k, v) -> write vec k (vconv v)
	freeze vec
