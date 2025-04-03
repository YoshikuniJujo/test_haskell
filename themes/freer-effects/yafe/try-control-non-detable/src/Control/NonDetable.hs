{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.NonDetable (N(..)) where

class N t where mz :: t a; mp :: t Bool
