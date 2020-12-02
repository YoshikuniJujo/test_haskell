{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools.Parse (Parse, parse, check, char, eof, (>*>)) where

import Control.Applicative (Alternative(..))
import Data.Maybe (listToMaybe)

newtype Parse a = Parse { runParse :: String -> [(a, String)] }

succeed :: a -> Parse a
succeed v = Parse \inp -> [(v, inp)]

check :: (Char -> Bool) -> Parse Char
check p = Parse \case c : cs | p c -> [(c, cs)]; _ -> []

char :: Char -> Parse Char
char = check . (==)

instance Functor Parse where
	f `fmap` Parse p = Parse \inp -> [ (f x, r) | (x, r) <- p inp ]

instance Applicative Parse where
	Parse pf <*> Parse px = Parse \inp ->
		[ (f x, r') | (f, r) <- pf inp, (x, r') <- px r ]
	pure = succeed

instance Alternative Parse where
	Parse p1 <|> Parse p2 = Parse \inp -> p1 inp ++ p2 inp
	empty = Parse \_ -> []

(>*>) :: Applicative f => f a -> f b -> f (a, b)
fx >*> fy = (,) <$> fx <*> fy

eof :: Parse ()
eof = Parse \case "" -> [((), "")]; _ -> []

parse :: Parse a -> String -> Maybe a
parse p = listToMaybe . map fst . ((p <* eof) `runParse`)
