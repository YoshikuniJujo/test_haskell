{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats

type family Foo (x :: Nat) where
	Foo 0 = ()
	Foo x = (,) Int (Foo (x - 1))
