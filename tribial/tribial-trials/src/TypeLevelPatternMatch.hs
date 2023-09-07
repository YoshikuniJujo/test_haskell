{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevelPatternMatch where

import Data.Kind

type family Test t where
	Test Int = 'True
	Test Integer = 'True
	Test t = 'False

data Bar a where
	Baz :: Bar Int
	Hoge :: Bar Double

class Foo (ts :: [Type]) (t :: Type) where
	type Boo ts
	foo :: String

{-
instance Foo (t ': ts) (Bar (Boo (t ': ts))) where
	type Boo (t ': ts) = t
	foo = "Int or Double"

instance {-# OVERLAPPABLE #-} Foo (t ': ts) t' where
	foo = "not Int nor Double"
	-}
