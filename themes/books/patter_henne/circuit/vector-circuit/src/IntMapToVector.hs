{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IntMapToVector where

import Control.Monad.ST
import Data.Foldable
import Data.Vector.Unboxed
import Data.Vector.Unboxed.Mutable

import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed.Mutable as V

intMapToVector :: Unbox v' => v' -> (v -> v') -> Int -> IM.IntMap v -> Vector v'
intMapToVector i vconv sz m = runST $ do
	vec <- sz `V.replicate` i
	for_ (IM.toList m) $ \(k, v) -> write vec k (vconv v)
	freeze vec
