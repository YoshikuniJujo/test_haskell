{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Foo
	= One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
	| Eleven | Twelve | Thirteen
	deriving (Show, Eq, Ord, Enum)

addFoo :: Foo -> Foo -> Foo
addFoo f One = succ f
addFoo f1 f2 = addFoo (succ f1) (pred f2)

subFoo :: Foo -> Foo -> Foo
subFoo f One = pred f
subFoo f1 f2 = subFoo (succ f1) (succ f2)

mulFoo :: Foo -> Foo -> Foo
mulFoo f One = f
mulFoo f1 f2 = f1 `addFoo` mulFoo f1 (pred f2)

instance Num Foo where
	(+) = addFoo
	(-) = subFoo
	(*) = mulFoo
	abs = id
	signum _ = 1
	fromInteger = toEnum . fromIntegral . subtract 1
