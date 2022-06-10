{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Text.Read

main :: IO ()
main = do
	putStrLn "何個?"
	n_ <- getLine
	case readMaybe n_ of
		Just n -> print $ take n chokkakutachi
		Nothing -> error "数字を入れてくれ"

check :: Int -> Int -> Int -> Bool
check a b c = a * a + b * b == c * c

all3 :: Int -> [(Int, Int, Int)]
all3 n = [ (a, b, c) |
	a <- [1 .. n - 2], b <- [a .. n - a - 1], let c = n - a - b ]

chokkakutachi :: [(Int, Int, Int)]
chokkakutachi = [ (a, b, c) | n <- [3 ..], (a, b, c) <- all3 n, check a b c ]
