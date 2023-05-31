module RecursionOverModules.List where

data List a

instance Show a => Show (List a)

len :: List a -> Int
