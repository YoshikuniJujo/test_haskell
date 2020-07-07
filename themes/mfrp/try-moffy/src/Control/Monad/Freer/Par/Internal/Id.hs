{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.Internal.Id (Id(..)) where

import Numeric.Natural

newtype Id = Id Natural deriving (Show, Eq)
