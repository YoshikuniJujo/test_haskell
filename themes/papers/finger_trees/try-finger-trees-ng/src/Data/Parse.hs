{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse (Parse(..), (>>!), maybeToParse) where

import Control.Arrow (first)
import Control.Applicative (Alternative(..))

newtype Parse s a = Parse { runParse :: s -> Maybe (a, s) }

instance Functor (Parse s) where
	f `fmap` Parse p = Parse \s -> (f `first`) <$> p s

instance Applicative (Parse t) where
	pure x = Parse \s -> Just (x, s)
	Parse pf <*> mx =
		Parse \s -> pf s >>= \(f, s') -> (f <$> mx) `runParse` s'

instance Monad (Parse t) where
	Parse p >>= f = Parse \s -> p s >>= \(x, s') -> f x `runParse` s'

instance Alternative (Parse t) where
	empty = Parse \_ -> Nothing
	Parse l <|> Parse r = Parse \s -> l s <|> r s

(>>!) :: Parse s a -> Parse s b -> Parse s a
Parse p >>! Parse nla =
	Parse \s -> p s >>= \r@(_, s') -> maybe (pure r) (const empty) $ nla s'

maybeToParse :: Maybe a -> Parse s a
maybeToParse mx = Parse \s -> (, s) <$> mx
