module Data.Pipe.List (
	fromList,
	toList,
	) where

import Control.Monad
import Data.Pipe

fromList :: (Monad m, Monad (p () a m), PipeClass p) => [a] -> p () a m ()
fromList = foldr ((>>) . yield) (return ())

-- | Consume all values from the stream and return as a list.
-- This will pull all values into memory.

toList :: (Monad m, Monad (p a () m), PipeClass p) => p a () m [a]
toList = do
	mx <- await
	case mx of
		Just x -> (x :) `liftM` toList
		_ -> return []
