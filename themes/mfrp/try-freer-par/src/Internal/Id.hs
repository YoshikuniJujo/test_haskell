{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Id (Id(..)) where

import Numeric.Natural

newtype Id = Id Natural deriving (Show, Eq)
