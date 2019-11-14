{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.ShowLazyList (showLazyList) where

import GHC.ClosureType (isEvaluated)
import Control.Arrow (second)
import Data.Bool (bool)
import System.IO.Unsafe (unsafePerformIO)

showLazyList :: Show a => [a] -> (Bool, [String])
showLazyList xs = unsafePerformIO . (<$> isEvaluated xs) . bool (False, [])
	$ case xs of
		[] -> (True, [])
		x : xs' -> (show x :) `second` showLazyList xs'
