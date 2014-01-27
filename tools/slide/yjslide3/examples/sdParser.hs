{-# LANGUAGE TupleSections #-}

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Monad

import Control.Category
import Control.Arrow

data StaticParser s = SP Bool [s]
newtype DynamicParser s a = DP ([s] -> (s, [s]))
data Parser s a = P (StaticParser s) (DynamicParser s a)

symbol :: s -> Parser s s
symbol s = P (SP False [s]) (DP $ (s ,) . tail)
