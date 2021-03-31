{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

class SomeClass a where

class Foo a where

class Bar a where

instance {-# INCOHERENT #-} Foo a => SomeClass a where

-- instance {-# INCOHERENT #-} Bar a => SomeClass a where

class OtherClass a where

class OtherClass a => Foo' a where

class OtherClass a => Bar' a where
