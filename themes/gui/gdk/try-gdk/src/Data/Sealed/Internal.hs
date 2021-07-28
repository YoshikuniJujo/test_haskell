module Data.Sealed.Internal (Sealed, seal, unsafeUnseal) where

newtype Sealed s a = Sealed a deriving Show

seal :: a -> Sealed s a
seal = Sealed

unsafeUnseal :: Sealed s a -> a
unsafeUnseal (Sealed x) = x
