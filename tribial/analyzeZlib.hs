{-# LANGUAGE LambdaCase, OverloadedStrings, BinaryLiterals, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad (guard)
import Control.Arrow (first, (***), (&&&))
import Data.Maybe (fromMaybe)
import Data.List (intercalate, foldl', scanl' )
import Data.Bits (testBit, (.&.), shiftR)
import Data.Bool (bool)
import Data.Word (Word8, Word16, Word32)
import Data.Char (chr)
import Data.String (IsString(..))
import System.Environment (getArgs)

import qualified Data.ByteString as BS
import qualified Deflate

-- MAIN FUNCTIONS

main :: IO ()
main = exMain . head =<< getArgs

exMain :: FilePath -> IO ()
exMain fp = maybe (putStrLn "bad zlib structure") BS.putStr
	. expandZlib =<< BS.readFile fp

expandZlib :: BS.ByteString -> Maybe BS.ByteString
expandZlib = (snd <$>) . analyzeZlib

analyzeZlib :: BS.ByteString -> Maybe (ZlibHeader, BS.ByteString)
analyzeZlib bs = do
	(zh, bs') <- zlibHeader bs
	(pln, ad0) <- Deflate.uncompress bs' -- analyzeBlocks `runStateT` toBitArray bs'
	guard $ adler32String pln == ad0
	return (zh, pln)

-- ZLIB HEADER

zlibHeader :: BS.ByteString -> Maybe (ZlibHeader, BS.ByteString)
zlibHeader bs = do
	((cm, ci), ((pd, cl), bs')) <- secondM flag =<< cmf bs
	return (ZlibHeader cm ci pd cl, bs')

checkCmfFlag :: BS.ByteString -> Bool
checkCmfFlag bs = fromMaybe False $ do
	(c, bs') <- BS.uncons bs
	(f, _) <- BS.uncons bs'
	return $ (fromIntegral c * 256 + fromIntegral f) `mod` 31 ==
		(0 :: Word16)

cmf :: BS.ByteString -> Maybe ((CompMethod, CompInfo), BS.ByteString)
cmf bs = do
	guard $ checkCmfFlag bs
	first ((toCompMethod *** toCompInfo) . word4s) <$> BS.uncons bs

flag :: BS.ByteString -> Maybe ((PreDict, CompLevel), BS.ByteString)
flag bs = first (toPreDict &&& toCompLevel) <$>  BS.uncons bs

data ZlibHeader = ZlibHeader {
	_compMethod :: CompMethod,
	_compInfo :: CompInfo,
	_preDict :: PreDict,
	_compLevel :: CompLevel
	} deriving Show

data CompMethod = CMDeflate | OtherCompMethod Word8 deriving Show

toCompMethod :: Word8 -> CompMethod
toCompMethod = \case 8 -> CMDeflate; w -> OtherCompMethod w

newtype CompInfo = CIWindowSize Word32 deriving Show

toCompInfo :: Word8 -> CompInfo
toCompInfo w = CIWindowSize $ 2 ^ (w + 8)

data PreDict = NotUsePreDict | UsePreDict deriving Show

toPreDict :: Word8 -> PreDict
toPreDict w = bool NotUsePreDict UsePreDict $ w `testBit` 5

newtype CompLevel = CompLevel Word8 deriving Show

toCompLevel :: Word8 -> CompLevel
toCompLevel = CompLevel . (`shiftR` 6)

-- DEFLATE

-- deflateHeader :: BitArray -> ((BFinal, BType), BitArray)

data BFinal = BNotFinal | BFinal deriving Show

data BType =
	NoCompression | FixedHuffman | DynamicHuffman | BTypeError
	deriving (Show, Eq)

-- BITARRAY

data BitArray = BitArray Word8 BS.ByteString deriving Show

newtype Bs = Bs { unBs :: [Bool] } deriving Eq

instance Show Bs where
	show = map (bool '0' '1') . unBs

instance Monoid Bs where
	mempty = Bs []
	Bs b1 `mappend` Bs b2 = Bs $ b1 ++ b2

instance IsString Bs where
	fromString = Bs . map (== '1')

-- DYNAMIC

newtype HCLen = HCLen { _unHCLen :: Word8 } deriving Show

newtype TOTContents = TOTContents Word8 deriving Show
newtype TableOfTable = TableOfTable [Word8]

instance Show TableOfTable where
	show (TableOfTable ws) = intercalate ", " . map
		(\(s, w) -> show s ++ ": " ++ show w)
		$ zip [
			16 :: Word8, 17, 18, 0, 8, 7, 9, 6,
			10, 5, 11, 4, 12, 3, 13, 2,
			14, 1, 15 ] ws

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
	fmap f (Leaf x) = Leaf $ f x
	fmap f (Node l r) = Node (fmap f l) (fmap f r)

newtype LitLenGen = LitLenGen Word16 deriving (Show, Eq)

data LitLen = Lit Word8 | EndOfBlock | Len Word16

instance Show LitLen where
	show (Lit c) = '\'' : chr (fromIntegral c) : "'"
	show EndOfBlock = "EOB"
	show (Len l) = "#" ++ show l

data LitLenClass
	= CLiteral
	| CEndOfBlock
	| CLen3_10
	| CLen11_18
	| CLen19_34
	| CLen35_66
	| CLen67_130
	| CLen131_257
	| CLen258
	| LitLenClassError
	deriving (Show, Eq)

newtype DistGen = DistGen Word16 deriving (Show, Eq)

newtype DistClass = CDist Word16

instance Show DistClass where
	show (CDist 1) = "CDist1_4"
	show (CDist w) = "CDist" ++
		show ((2 :: Word16) ^ w + 1) ++ "_" ++
		show ((2 :: Word16) ^ (w + 1))

newtype Dist = Dist Word16 deriving Show

data LitLenDist
	= LldLit Word8
	| LldLenDist { _lldLen :: Word16, _lldDist :: Word16 }
	deriving Show

step :: Integral a => Word32 -> a -> Word32
step w1 w2 = (w1 + fromIntegral w2) `mod` 65521

adler32String :: BS.ByteString -> BS.ByteString
adler32String str = let
	ns = scanl' step 1 $ BS.unpack str
	w1 = last ns
	w2 = foldl' step 0 (tail ns) in BS.pack $ map fromIntegral [
		w2 `shiftR` 8, w2 .&. 0xff,
		w1 `shiftR` 8, w1 .&. 0xff ]

-- TOOLS

word4s :: Word8 -> (Word8, Word8)
word4s w = (w .&. 0b1111, w `shiftR` 4)

secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f = uncurry (<$>) . ((,) *** f)
