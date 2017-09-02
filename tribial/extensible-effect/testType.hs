{-# LANGUAGE FlexibleContexts #-}

import TypeElem

data MyMonad r a = MyMonad a deriving Show

instance Functor (MyMonad r) where

instance Applicative (MyMonad r) where

instance Monad (MyMonad r) where

myCheck :: Monoid r => MyMonad r Integer
myCheck = MyMonad 888

check2 :: Monoid r => MyMonad r Integer
check2 = MyMonad 123

check3 :: Member [] r => MyMonad r Integer
check3 = MyMonad 456
