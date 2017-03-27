{-# LANGUAGE DeriveGeneric, EmptyDataDecls #-}

import GHC.Generics

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Show, Generic)

data Hoge = Hoge | Hige | Huge deriving (Show, Generic)

data Foo deriving Generic
