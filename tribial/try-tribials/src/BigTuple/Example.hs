{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BigTuple.Example where

import Data.Char

data Tuple3 a b c = Tuple3 a b c deriving Show

foo :: Tuple3 Int Char () -> Tuple3 Int Char ()
foo (Tuple3 x c u) = Tuple3 (x + 1) (toUpper c) u
