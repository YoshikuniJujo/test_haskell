{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse (Parse, parse, unparse) where

import Control.Monad.State

type Parse d a = StateT d Maybe a

parse :: (d -> Maybe (a, d)) -> Parse d a
parse = StateT

unparse :: Parse d a -> d -> Maybe (a, d)
unparse = runStateT
