module Tools where

import Data.Array

listToArray :: (Ix i, Integral i) => [e] -> Array i e
listToArray xs = listArray (0, fromIntegral (length xs) - 1) xs
