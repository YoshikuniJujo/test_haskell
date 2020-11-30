{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
 
module Data.View where

data ViewL s a = NL | ConsL a (s a) deriving Show

data ViewR s a = NR | ConsR (s a) a deriving Show
