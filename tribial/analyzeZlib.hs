{-# LANGUAGE
	LambdaCase, OverloadedStrings, BinaryLiterals, TupleSections
	#-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Monad
import Control.Arrow
import Data.Bool
import Data.Maybe
import Data.List
import Data.Bits
import Data.Word
import Data.String

import qualified Data.ByteString as BS

-- ZLIB HEADER

zlibHeader :: BS.ByteString -> Maybe (ZlibHeader, BS.ByteString)
zlibHeader bs = do
	((cm, ci), ((pd, cl), bs')) <- secondM flag =<< cmf bs
	return (ZlibHeader cm ci pd cl, bs')

checkCmfFlag :: BS.ByteString -> Bool
checkCmfFlag bs = fromMaybe False $ do
	(c, bs') <- BS.uncons bs
	(f, _) <- BS.uncons bs'
	return $ (fromIntegral c * 256 + fromIntegral f) `mod` 31 == 0

cmf :: BS.ByteString -> Maybe ((CompMethod, CompInfo), BS.ByteString)
cmf bs = do
	guard $ checkCmfFlag bs
	first ((toCompMethod *** toCompInfo) . word4s) <$> BS.uncons bs

flag :: BS.ByteString -> Maybe ((PreDict, CompLevel), BS.ByteString)
flag bs = first (toPreDict &&& toCompLevel) <$>  BS.uncons bs

data ZlibHeader = ZlibHeader {
	compMethod :: CompMethod,
	compInfo :: CompInfo,
	preDict :: PreDict,
	compLevel :: CompLevel
	} deriving Show

data CompMethod = CMDeflate | OtherCompMethod Word8 deriving Show

toCompMethod :: Word8 -> CompMethod
toCompMethod = \case 8 -> CMDeflate; w -> OtherCompMethod w

data CompInfo = CIWindowSize Word32 deriving Show

toCompInfo :: Word8 -> CompInfo
toCompInfo w = CIWindowSize $ 2 ^ (w + 8)

data PreDict = NotUsePreDict | UsePreDict deriving Show

toPreDict :: Word8 -> PreDict
toPreDict w = bool NotUsePreDict UsePreDict $ w `testBit` 5

data CompLevel = CompLevel Word8 deriving Show

toCompLevel :: Word8 -> CompLevel
toCompLevel = CompLevel . (`shiftR` 6)

secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f = uncurry (<$>) . ((,) *** f)

-- DEFLATE

-- deflateHeader :: BitArray -> ((BFinal, BType), BitArray)

headerBits :: BitArray -> Maybe (HeaderBits, BitArray)
headerBits ba = do
	(bf, ba') <- bfinal ba
	(bt, ba'') <- btype ba'
	return (HeaderBits bf bt, ba'')

data HeaderBits = HeaderBits {
	bFinal :: BFinal,
	bType :: BType
	} deriving Show

data BFinal = BNotFinal | BFinal deriving Show

bfinal :: BitArray -> Maybe (BFinal, BitArray)
bfinal ba = first (bool BNotFinal BFinal) <$> unconsBits ba

data BType =
	NoCompression | FixedHuffman | DynamicHuffman | BTypeError
	deriving Show

btype :: BitArray -> Maybe (BType, BitArray)
btype ba = do
	(b0, ba') <- unconsBits ba
	(b1, ba'') <- unconsBits ba'
	return (
		case (b1, b0) of
			(False, False) -> NoCompression
			(False, True) -> FixedHuffman
			(True, False) -> DynamicHuffman
			(True, True) -> BTypeError,
		ba'' )

-- BITARRAY

data BitArray = BitArray Word8 BS.ByteString deriving Show

toBitArray :: BS.ByteString -> BitArray
toBitArray = BitArray 0

unconsBits :: BitArray -> Maybe (Bool, BitArray)
unconsBits (BitArray _ "") = Nothing
unconsBits (BitArray i bs) = do
	(b, bs') <- BS.uncons bs
	return (b `testBit` fromIntegral i, if i < 7
		then BitArray (i + 1) bs
		else BitArray 0 bs')

data Bs = Bs { unBs :: [Bool] } deriving Eq

popBit :: Bs -> (Bool, Bs)
popBit (Bs []) = error "empty Bits"
popBit (Bs (b : bs)) = (b, Bs bs)

pushBit :: Bool -> Bs -> Bs
pushBit b = Bs . (b :) . unBs

bitsToWord :: (Bits a, Num a) => Bs -> a
bitsToWord (Bs []) = 0
bitsToWord bs = let (b, bs') = popBit bs in
	bool 0 1 b + (bitsToWord bs' `shiftL` 1)

instance Show Bs where
	show = map (bool '0' '1') . unBs

instance Monoid Bs where
	mempty = Bs []
	Bs b1 `mappend` Bs b2 = Bs $ b1 ++ b2

instance IsString Bs where
	fromString = Bs . map (== '1')

popBits :: Word8 -> BitArray -> Maybe (Bs, BitArray)
popBits n ba | n <= 0 = Just (mempty, ba)
popBits n ba = do
	(b, ba') <- unconsBits ba
	(bs, ba'') <- popBits (n - 1) ba'
	return (b `pushBit` bs, ba'')

initBits :: Bs -> Bs
initBits = Bs . init . unBs

reverseBits :: Bs -> Bs
reverseBits = Bs . reverse . unBs

-- FIXED HUFFMAN

adhocFixed :: BitArray -> BS.ByteString
adhocFixed = (BS.pack .) . unfoldr $ \a -> do
	(bs, a') <- popBits 8 a
	w <- adhocFixed1 bs
	return (w, a')

adhocFixed1 :: Bs -> Maybe Word8
adhocFixed1 bs_ = do
	guard $ initBits bs /= "0000000"
	return $ bitsToWord bs - 48
	where bs = reverseBits bs_

-- TOOLS

bits :: BS.ByteString -> String
bits = concatMap bits8 . BS.unpack

bits8 :: Word8 -> String
bits8 w = map (bool '0' '1' . (w `testBit`)) [0 .. 7]

word4s :: Word8 -> (Word8, Word8)
word4s w = (w .&. 0b1111, w `shiftR` 4)

getDeflate :: BS.ByteString -> BS.ByteString
getDeflate = BS.reverse . BS.drop 4 . BS.reverse . BS.drop 2
