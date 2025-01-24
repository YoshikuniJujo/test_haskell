module Functor.Unit where

data Unit a = Unit

instance Functor Unit where _ `fmap` _ = Unit
