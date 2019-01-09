{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word

data Bit = O | I deriving Show

fromString :: String -> [Bit]
fromString = map fc
	where fc '0' = O; fc '1' = I; fc _ = error "Oops!"

toWord8 :: Int -> Int -> [Bit] -> Word8
toWord8 mn _ [] = fromIntegral mn
toWord8 mn mx (O : bs) = toWord8 mn ((mn + mx) `div` 2) bs
toWord8 mn mx (I : bs) = toWord8 ((mn + mx) `div` 2) mx bs

fromWord8 :: Int -> Int -> Word8 -> [Bit]
fromWord8 mn mx _ | mn + 1 >= mx = []
fromWord8 mn mx w
	| fromIntegral w < sp = O : fromWord8 mn sp w
	| otherwise = I : fromWord8 sp mx w
	where sp = (mn + mx) `div` 2
