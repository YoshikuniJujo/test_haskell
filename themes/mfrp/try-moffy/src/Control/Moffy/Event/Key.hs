{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key where

import Data.Word

newtype Key = Key Word32 deriving (Show, Eq)
