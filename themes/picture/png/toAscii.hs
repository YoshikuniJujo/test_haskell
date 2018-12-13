{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude as P

import Data.Vector.Storable
import Data.Word
import System.IO.Unsafe
import Codec.Picture

main :: IO ()
main = putStr aa

w, h :: Int
v :: Vector Word8
Right (ImageY8 (Image w h v)) =
	unsafePerformIO $ readImage "png-files/mini_lena.png"

img :: [[Word8]]
img = groupsN w $ toList v

aa :: String
aa = unlines $ P.map
	(P.map $ toChar (P.minimum cimg) (P.maximum cimg))
--	(P.map $ toChar 0 255)
	img
	where
	cimg = P.concat img

groupsN :: Int -> [a] -> [[a]]
groupsN _ [] = []
groupsN n xs = P.take n xs : groupsN n (P.drop n xs)

toChar :: Word8 -> Word8 -> Word8 -> Char
toChar mn mx n
--	| n > 204 = '$'
--	| n > 153 = '='
--	| n > 102 = '\\'
--	| n > 51 = '.'
--	| otherwise = ' '
	| n > b 4 = '$'
	| n > b 3 = '='
	| n > b 2 = '\\'
	| n > b 1 = '.'
	| otherwise = ' '
	where
	b i = mn + fromIntegral ((fromIntegral (mx - mn) :: Int) * i `div` 5)
--	b i = 0 + fromIntegral ((255 - 0) * i :: Int) `div` 5
