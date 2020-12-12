{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse where

import Control.Arrow
import Control.Applicative

newtype Parse d a = Parse { runParse :: d -> Maybe (a, d) }

parse :: (d -> Maybe (a, d)) -> Parse d a
parse = Parse

instance Functor (Parse t) where
	f `fmap` Parse p = Parse \ts -> (f `first`) <$> p ts

instance Applicative (Parse t) where
	pure x = Parse \ts -> Just (x, ts)
	Parse mf <*> mx =
		Parse \ts -> mf ts >>= \(f, ts') -> (f <$> mx) `runParse` ts'

instance Monad (Parse t) where
	Parse p >>= f = Parse \ts -> p ts >>= \(x, ts') -> f x `runParse` ts'

instance Alternative (Parse t) where
	empty = Parse \_ -> Nothing
	Parse p1 <|> Parse p2 = Parse \ts -> p1 ts <|> p2 ts
