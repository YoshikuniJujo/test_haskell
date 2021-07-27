module Data.Sealed.Internal (Sealed(..)) where

newtype Sealed s a = Sealed a deriving Show
