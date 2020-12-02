{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools.Parse (Parse, parse, check, token, eof, (>*>)) where

import Control.Applicative (Alternative(..))
import Data.Maybe (listToMaybe)

newtype Parse t a = Parse { runParse :: [t] -> [(a, [t])] }

succeed :: a -> Parse t a
succeed v = Parse \inp -> [(v, inp)]

check :: (t -> Bool) -> Parse t t
check p = Parse \case c : cs | p c -> [(c, cs)]; _ -> []

token :: Eq t => t -> Parse t t
token = check . (==)

instance Functor (Parse t) where
	f `fmap` Parse p = Parse \inp -> [ (f x, r) | (x, r) <- p inp ]

instance Applicative (Parse t) where
	Parse pf <*> Parse px = Parse \inp ->
		[ (f x, r') | (f, r) <- pf inp, (x, r') <- px r ]
	pure = succeed

instance Alternative (Parse t) where
	Parse p1 <|> Parse p2 = Parse \inp -> p1 inp ++ p2 inp
	empty = Parse \_ -> []

(>*>) :: Applicative f => f a -> f b -> f (a, b)
fx >*> fy = (,) <$> fx <*> fy

eof :: Parse t ()
eof = Parse \case [] -> [((), [])]; _ -> []

parse :: Parse t a -> [t] -> Maybe a
parse p = listToMaybe . map fst . ((p <* eof) `runParse`)
