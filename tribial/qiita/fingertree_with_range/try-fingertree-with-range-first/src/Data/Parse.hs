module Data.Parse (Parse, parse, unparse) where

import Control.Monad.State

type Parse s = StateT s Maybe

parse :: (s -> Maybe (a, s)) -> Parse s a
parse = StateT

unparse :: Parse s a -> s -> Maybe (a, s)
unparse = runStateT
