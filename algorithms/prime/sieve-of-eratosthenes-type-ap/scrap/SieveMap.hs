{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SieveMap where

import Control.Monad.Identity
import Data.Ix
import Data.Map
import Data.Array.MArray
import Data.Array.Base (getNumElements, unsafeRead, unsafeWrite)

import SieveGen

data MapArray i e = MapArray (i, i) (Map Int e) deriving Show

instance MArray MapArray e Identity where
	getBounds (MapArray (mn, mx) _) = return (mn, mx)
	getNumElements (MapArray (mn, mx) _) = return $ rangeSize (mn, mx)
	newArray bs iv = return
		. MapArray bs . fromList $ zip [0 .. rangeSize bs] (repeat iv)
	unsafeRead (MapArray _ mp) i = return $ mp ! i
