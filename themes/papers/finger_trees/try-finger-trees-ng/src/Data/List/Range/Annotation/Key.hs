{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.Annotation.Key where

import Data.FingerTree.Annotated

data Key a = NoKey | Key a deriving (Show, Eq, Ord)

instance Semigroup (Key a) where k <> NoKey = k; _ <> k = k
instance Monoid (Key a) where mempty = NoKey

instance Measured (Elem a) (Key a) where measure (Elem x) = Key x
