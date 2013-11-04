{-# LANGUAGE TypeFamilies #-}

import Data.Word
import Data.Bits
import Data.Either
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C (pack)

data family Pack a

data instance Pack Bool = PackBool [Word8] [Bool]
	deriving Show

data instance Pack Char = PackChar ByteString
	deriving Show

packBool8 :: [Bool] -> Word8
packBool8 [] = 0
packBool8 (False : bs) = shiftL (packBool8 bs) 1
packBool8 (True : bs) = shiftL (packBool8 bs) 1 .|. 1

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

eitherPack :: [Bool] -> Either [Bool] Word8
eitherPack bs
	| length bs == 8 = Right $ packBool8 bs
	| otherwise = Left bs

packBool :: [Bool] -> Pack Bool
packBool bs = let (bs', ws) =  partitionEithers $ map eitherPack (groupN 8 bs) in
	PackBool ws $ concat bs'

packChar :: [Char] -> Pack Char
packChar = PackChar . C.pack

class Packable a where
	pack :: [a] -> Pack a

instance Packable Bool where
	pack = packBool

instance Packable Char where
	pack = packChar
