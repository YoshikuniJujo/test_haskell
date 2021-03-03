{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Title where

import Data.Char

titleWidth :: Int
titleWidth = 80

titleWidthHalf1, titleWidthHalf2 :: Int
titleWidthHalf1 = titleWidth `div` 2
titleWidthHalf2 = titleWidth - titleWidthHalf1

mkTitle :: String -> String
mkTitle ttl =
	replicate titleWidth '*' ++ "\n" ++
	"*" ++ replicate (titleWidthHalf1 - 1 - s2) ' ' ++
		ttl' ++ replicate (titleWidthHalf2 - 1 - s1) ' ' ++ "*\n" ++ 
	replicate titleWidth '*' ++ "\n"
	where
	ttl' = toUpper <$> ttl
	s = length ttl; s1 = s `div` 2; s2 = s - s1
