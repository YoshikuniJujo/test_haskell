{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ShowLazyList where

import GHC.Exts.Heap (GenClosure(..), getClosureData)
import Control.Arrow
import Data.Bool

isEvaluated :: a -> IO Bool
isEvaluated x = (<$> getClosureData x) $ \case
	ConstrClosure {} -> True
	BlackholeClosure {} -> True
	_ -> False

showLazyList :: Show a => [a] -> IO (Bool, [String])
showLazyList xs = (=<< isEvaluated xs) . bool (return (False, []))
	$ case xs of
		[] -> return (True, [])
		x : xs' -> second (show x :) <$> showLazyList xs'
