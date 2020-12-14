{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse (Parse, parse, unparse, (>>!), maybeToParse) where

import Control.Applicative (empty)
import Control.Monad ((>=>))
import Control.Monad.State (StateT(..))

type Parse s a = StateT s Maybe a

parse :: (s -> Maybe (a, s)) -> Parse s a
parse = StateT

unparse :: Parse s a -> s -> Maybe (a, s)
unparse = runStateT

maybeToParse :: Maybe a -> Parse s a
maybeToParse mx = parse \s -> (, s) <$> mx

(>>!) :: Parse s a -> Parse s b -> Parse s a
p >>! nla = parse $ unparse p >=> \r@(_, s') ->
	maybe (pure r) (const empty) $ unparse nla s'
