{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data FooInGadtClothing a where
	MkFooInGadtClothing :: a -> FooInGadtClothing a

data TrueGadtFoo a where
	MkTrueGadtFoo :: a -> TrueGadtFoo Int

data Bar where
	BarNone :: Bar

data Foo where
	MkFoo :: Bar
