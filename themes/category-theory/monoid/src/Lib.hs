module Lib where

import Data.Monoid hiding (Endo)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a)

instance Monoid (Endo a) where
	mempty = Endo id
	Endo f `mappend` Endo g = Endo $ f . g

(|$|) :: Endo a -> a -> a
Endo f |$| x = f x
