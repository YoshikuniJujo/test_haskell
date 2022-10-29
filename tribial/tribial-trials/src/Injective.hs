{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Injective where

import Data.Proxy

type family Zip as bs = r | r -> as bs where
	Zip '[] '[] = '[]
	Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs

class Foo as bs where
	foo :: Proxy (Zip as bs) -> Int

instance Foo '[Int] '[Bool] where
	foo Proxy = 123
