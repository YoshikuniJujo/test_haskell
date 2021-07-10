module Inits where

import Data.List.NonEmpty

import Inits1

inits :: [a] -> [[a]]
inits xs = [] : (toList <$> inits1 xs)
