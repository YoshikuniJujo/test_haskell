{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

main :: IO ()
main = print $ mySum [123, 456, 789]

mySum :: [Int] -> Int
mySum = sm 0
	where
	sm s [] = s
	sm s (n : ns) = let s' = s + n in s' `seq` sm s' ns
