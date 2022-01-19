{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-} -- , MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckOverlappable where

class Foo a b where
	foo :: a -> b -> Int

instance Foo a a where
	foo _ _ = 123

instance {-# OVERLAPPABLE #-} Foo a b where
	foo _ _ = 321

class Bar a b where
	bar :: a -> b -> Int

instance Bar a b where
	bar = foo

baz :: Foo a b => a -> b -> Int
baz = foo

class Qux a b where
	qux :: a -> b -> Int

instance Foo a b => Qux a b where
	qux = foo
