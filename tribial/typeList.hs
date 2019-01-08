{-# LANGUAGE
	TypeFamilies, DataKinds, FlexibleInstances, TypeOperators,
	FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Mod = A | B | C deriving Show

data family Foo (x :: [Mod])

data instance Foo '[] = N
data instance Foo ('A : ms) = FA (Foo ms)
data instance Foo ('B : ms) = FB (Foo ms)
data instance Foo ('C : ms) = FC (Foo ms)

class Bar x where
	bar :: x -> String

instance Bar (Foo '[]) where
	bar _ = ""

instance Bar (Foo ms) => Bar (Foo ('A ': ms)) where
	bar (FA x) = 'a' : bar x

instance Bar (Foo ms) => Bar (Foo ('B ': ms)) where
	bar (FB x) = 'b' : bar x

instance Bar (Foo ms) => Bar (Foo ('C ': ms)) where
	bar (FC x) = 'c' : bar x
