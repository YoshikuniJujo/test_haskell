{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

import Data.Word
import Data.Bits
import Data.Either
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C (pack)

import Data.Char

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

class Some a where
	data Hoge a

-- data instance Hoge Char = Int

data family Identity a

data instance Identity Int = PrimeFactors [Int]
	deriving Show

primeFactors :: Int -> Int -> [Int]
primeFactors f n
	| f <= 0 || n <= 0 = []
	| f > n = []
	| n `mod` f == 0 = f : primeFactors f (n `div` f)
	| otherwise = primeFactors (f + 1) n

toIdentityInt :: Int -> Maybe (Identity Int)
toIdentityInt n
	| n > 0 = Just $ PrimeFactors $ primeFactors 2 n
	| otherwise = Nothing

fromIdentityInt :: Identity Int -> Int
fromIdentityInt (PrimeFactors pfs) = product pfs

data CharClass = Upper | Lower | Digit deriving Show

data instance Identity Char = CharID CharClass Int
	deriving Show

toIdentityChar :: Char -> Maybe (Identity Char)
toIdentityChar c
	| isUpper c = Just $ CharID Upper $ ord c - ord 'A'
	| isLower c = Just $ CharID Lower $ ord c - ord 'a'
	| isDigit c = Just $ CharID Digit $ ord c - ord '0'
	| otherwise = Nothing

fromIdentityChar :: Identity Char -> Char
fromIdentityChar (CharID Upper n) = chr $ ord 'A' + n
fromIdentityChar (CharID Lower n) = chr $ ord 'a' + n
fromIdentityChar (CharID Digit n) = chr $ ord '0' + n

class HaveIdentity a where
	toIdentity :: a -> Maybe (Identity a)
	fromIdentity :: Identity a -> a

instance HaveIdentity Int where
	toIdentity = toIdentityInt
	fromIdentity = fromIdentityInt

instance HaveIdentity Char where
	toIdentity = toIdentityChar
	fromIdentity = fromIdentityChar

printIdentity :: (HaveIdentity a, Show (Identity a)) => a -> IO ()
printIdentity = print . toIdentity
