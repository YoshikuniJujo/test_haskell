{-# LANGUAGE PatternSynonyms, TypeFamilies #-}

module B (pattern BarUnit) where

class Foo a where data Bar a
instance Foo () where data Bar () = BarUnit
