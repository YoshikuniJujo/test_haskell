{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

data a :+: b = a :+: b deriving Show

class Flatten src dst where
	flatten :: src -> dst

instance Flatten Int Int where flatten = id
instance Flatten () () where flatten = id

instance Flatten src dst => Flatten (t :+: src) (t :+: dst) where
	flatten (x :+: xs) = x :+: flatten xs

instance {-# OVERLAPPABLE #-} (Cat s1 s2' dst, Flatten s2 s2') =>
	Flatten (s1 :+: s2) dst where
	flatten (s1 :+: s2) = s1 `cat` (flatten s2 :: s2')

class Cat s1 s2 dst where
	cat :: s1 -> s2 -> dst

instance Cat Int s2 (Int :+: s2) where cat = (:+:)
instance Cat () s2 (() :+: s2) where cat = (:+:)

instance Cat s1 s2 dst => Cat (t :+: s1) s2 (t :+: dst) where
	cat (x :+: xs) ys = x :+: cat xs ys

instance {-# OVERLAPPABLE #-} (Cat s1 med dst, Cat s2 s3 med) =>
	Cat (s1 :+: s2) s3 dst where
	cat (xs :+: ys) zs = cat xs (cat ys zs :: med)

data Foo = Foo :+ Foo | Foo Int deriving Show

flattenFoo :: Foo -> Foo
flattenFoo i@(Foo _) = i
flattenFoo (i@(Foo _) :+ is) = i :+ flattenFoo is
flattenFoo (is1 :+ is2) = is1 `catFoo` flattenFoo is2

catFoo :: Foo -> Foo -> Foo
i@(Foo _) `catFoo` is2 = i :+ is2
(i1@(Foo _) :+ is1) `catFoo` is2 = i1 :+ (is1 `catFoo` is2)
(is1 :+ is2) `catFoo` is3 = is1 `catFoo` is2 `catFoo` is3
