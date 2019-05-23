module Tools where

import Circuit

run :: Int -> Circuit -> Circuit
run n = (!! n) . iterate step

listToTuple2 :: [a] -> (a, a)
listToTuple2 [a, b] = (a, b)
