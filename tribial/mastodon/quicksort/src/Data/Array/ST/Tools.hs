{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Array.ST.Tools where

import Control.Monad.ST
import Data.Array.ST

import qualified Data.Array.Tools as A

copy :: STArray s Int e -> Int -> Int -> ST s ()
copy = A.copy
