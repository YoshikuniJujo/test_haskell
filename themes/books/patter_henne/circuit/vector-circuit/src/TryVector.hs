{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryVector where

import Control.Monad.ST
import Data.Foldable
import Data.Map.Strict
import Data.Vector.Unboxed.Mutable
import Data.Vector.Unboxed

import qualified Data.Map.Strict as M

v1 :: Vector Char
v1 = runST $ do
	v <- new 7
	write v 3 'h'
	write v 0 'e'
	write v 2 'l'
	write v 1 'l'
	write v 6 'o'
	write v 5 ','
	write v 4 ' '
	freeze v

m1 :: Map Int Char
m1 = M.fromList [(3, 'h'), (2, 'l'), (0, 'q'), (1, 'p')]

v2 :: Vector Char
v2 = runST $ do
	vec <- new 4
	for_ (M.toList m1) $ \(k, v) -> write vec k v
	freeze vec
