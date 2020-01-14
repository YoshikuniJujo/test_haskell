{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DiffMonoid where

import Prelude hiding (abs)
import Data.String

newtype DiffMonoid a = DiffMonoid { runDiffMonid :: a -> a }

abs :: Monoid a => DiffMonoid a -> a
abs (DiffMonoid a) = a mempty

rep :: Monoid a => a -> DiffMonoid a
rep = DiffMonoid . mappend

instance Monoid a => Semigroup (DiffMonoid a) where
	DiffMonoid a <> DiffMonoid b = DiffMonoid $ a . b

instance Monoid a => Monoid (DiffMonoid a) where mempty = rep mempty

(++.) :: [a] -> [a] -> [a]
[] ++. ys = ys
(x : xs) ++. ys = x : (xs ++. ys)

newtype MyString = MyString String deriving Show

instance IsString MyString where fromString = MyString
instance Semigroup MyString where MyString s <> MyString t = MyString $ s ++. t
instance Monoid MyString where mempty = MyString ""

myLast :: MyString -> Char
myLast (MyString s) = last s

hello :: MyString
hello = "hello"

helloL, helloR :: DiffMonoid MyString
helloL = foldl (<>) mempty . replicate 100000 $ rep hello
helloR = foldr (<>) mempty . replicate 100000 $ rep hello

helloLString, helloRString :: MyString
helloLString = {-# SCC "LeftAssociatedHellos" #-} abs helloL
helloRString = {-# SCC "RightAssociatedHellos" #-} abs helloR
