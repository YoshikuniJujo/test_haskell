{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.NonDetable (NonDetable(..)) where

class NonDetable t where mz :: t a; mp :: t Bool
