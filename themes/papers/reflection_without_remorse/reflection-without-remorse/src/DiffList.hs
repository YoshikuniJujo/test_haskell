{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DiffList where

import Prelude hiding ((++), null, abs)
import qualified Prelude

import ListConcatenation

type DiffList a = [a] -> [a]

abs :: DiffList a -> [a]
abs a = a []

rep :: [a] -> DiffList a
rep = (++)

(++.) :: DiffList a -> DiffList a -> DiffList a
(++.) = (.)

null :: DiffList a -> Bool
null = Prelude.null . abs

(++..) :: DiffList a -> DiffList a -> DiffList a
xs ++.. ys
	| null xs = rep []
	| otherwise = xs ++. ys
