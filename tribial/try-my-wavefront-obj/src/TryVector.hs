{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryVector where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Vector.Storable
import Data.Vector.Storable.Mutable

foo :: (Vector Int, Vector Double)
foo = runST do
	i <- newSTRef 0
	v <- new 5
	v' <- new 8
	write v 3 8
	write v 4 5
	write v' 4 7
	write v' 2 9
	replicateM_ 3 do
		modifySTRef i (+ 1)
		(flip (write v) 123) =<< readSTRef i
	(,) <$> freeze v <*> freeze v'
