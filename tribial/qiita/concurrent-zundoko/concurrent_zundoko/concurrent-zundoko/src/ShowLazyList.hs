{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ShowLazyList (showLazyList) where

import GHC.Exts.Heap (GenClosure(..), getClosureData)
import Control.Arrow (second)
import Data.Bool (bool)

isEvaluated :: a -> IO Bool
isEvaluated x = (<$> getClosureData x) $ \case
	ConstrClosure {} -> True
	BlackholeClosure {} -> True
	_ -> False

showLazyList :: Show a => [a] -> IO (Bool, [String])
showLazyList xs = (=<< isEvaluated xs) . bool (pure (False, []))
	$ case xs of
		[] -> pure (True, [])
		x : xs' -> second (show x :) <$> showLazyList xs'
