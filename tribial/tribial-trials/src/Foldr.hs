module Foldr where

import Data.Bool

index :: Foldable t => t a -> Int -> a
xs `index` n | n >= 0 = foldr (\x r k -> case k of 0 -> x; _ -> r (k - 1)) undefined xs n
_ `index` _ = error "bad"
