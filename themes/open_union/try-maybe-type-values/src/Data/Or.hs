{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Or (Or(..), or) where

import Prelude hiding (or)

data Or a b = L a | R b | LR a b deriving Show

or :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Or a b -> c
or f g h = \case L x -> f x; R y -> g y; LR x y -> h x y
