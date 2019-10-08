{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures, StandaloneDeriving, ScopedTypeVariables  #-}

-- https://gist.github.com/phillopon/a3ff274b371b8c75033a

module Foo where

import GHC.TypeLits

data List :: Nat -> * -> * where
    Nil   :: List 0 a
    (:::) :: a -> List length a -> List (length + 1) a

infixr 5 :::

deriving instance Show a => Show (List l a)

(+++) :: List l a -> List m a -> List (l + m) a
Nil +++ ys = ys
(x:::xs) +++ ys = x ::: xs +++ ys

infixr 5 +++

head_ :: List (n + 1) a -> a
head_ (a ::: _) = a

last_ :: forall l a. List (l + 1) a -> a
last_ = go
  where
    go :: forall l. List l a -> a
    go (a ::: Nil) = a
    go (_ ::: as) = go as

tail_ :: List (n + 1) a -> List n a
tail_ (_ ::: as) = as

{-
init_ :: forall l a. List (l + 1) a -> List l a
init_ = go
  where
    go :: forall l. List l a -> List (l - 1) a
    go (_ ::: Nil) = Nil
    go (a ::: as) = a ::: go as
    -}

map_ :: (a -> b) -> List l a -> List l b
map_ f Nil = Nil
map_ f (x ::: xs) = f x ::: map_ f xs

reverse_ :: List l a -> List l a
reverse_ l = rev l Nil
  where
    rev :: List l a -> List m a -> List (l + m) a
    rev Nil a = a
    rev (x:::xs) a = rev xs (x:::a)

foldl_ :: forall l a b. (b -> a -> b) -> b -> List l a -> b
foldl_ f z0 xs0 = lgo z0 xs0
  where
    lgo :: forall l. b -> List l a -> b
    lgo z Nil = z
    lgo z (x ::: xs) = lgo (f z x) xs
