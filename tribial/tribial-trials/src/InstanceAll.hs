{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, TypeOperators, DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module InstanceAll where

import Data.Kind

class Foo a where foo :: a -> Int

instance Foo Int where foo = id
instance Foo Double where foo = round
instance Foo () where foo _ = 0

type family Tuple (ts :: [Type]) :: Type where
	Tuple '[] = ()
	Tuple (a ': as) = (a, Tuple as)

class FooAll (as :: [Type]) where
	bar :: Tuple as -> Int

instance FooAll '[] where bar _ = 0

instance (Foo a, FooAll as) => FooAll (a ': as) where
	bar (x, xs) = foo x + bar @as xs
