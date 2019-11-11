{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reimplementation.NeverOccur (neverOccur) where

import Control.Exception (Exception, throw)

data NeverOccur = NeverOccur deriving Show
instance Exception NeverOccur

neverOccur :: a
neverOccur = throw NeverOccur
