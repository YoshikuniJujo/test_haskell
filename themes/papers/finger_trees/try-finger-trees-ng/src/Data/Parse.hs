{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse (Parse(..), (>>!), maybeToParse) where

import Control.Arrow (first)
import Control.Applicative (Alternative(..))

newtype Parse d a = Parse { runParse :: d -> Maybe (a, d) }

instance Functor (Parse d) where
	f `fmap` Parse p = Parse \d -> (f `first`) <$> p d

instance Applicative (Parse t) where
	pure x = Parse \d -> Just (x, d)
	Parse pf <*> mx =
		Parse \d -> pf d >>= \(f, d') -> (f <$> mx) `runParse` d'

instance Monad (Parse t) where
	Parse p >>= f = Parse \d -> p d >>= \(x, d') -> f x `runParse` d'

instance Alternative (Parse t) where
	empty = Parse \_ -> Nothing
	Parse l <|> Parse r = Parse \d -> l d <|> r d

(>>!) :: Parse d a -> Parse d b -> Parse d a
Parse p >>! Parse nla =
	Parse \d -> p d >>= \r@(_, d') -> maybe (pure r) (const empty) $ nla d'

maybeToParse :: Maybe a -> Parse d a
maybeToParse mx = Parse \d -> (, d) <$> mx
