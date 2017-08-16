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
import Data.Char
import Data.String
import Data.Function

import qualified Data.ByteString as BS

-- AD HOC

adhocDropHeaders :: BS.ByteString -> BitArray
adhocDropHeaders bs = fromJust $ do
	(_, bs') <- zlibHeader bs
	(_, ba) <- headerBits $ toBitArray bs'
	return ba

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

checkFixedOr :: BS.ByteString -> Maybe BType
checkFixedOr bs = do
	(_, bs') <- zlibHeader bs
	(HeaderBits _ bt, _) <- headerBits $ toBitArray bs'
	return bt

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

nullBitArray :: BitArray -> Bool
nullBitArray (BitArray _ bs) = BS.null bs

toBitArray :: BS.ByteString -> BitArray
toBitArray = BitArray 0

unconsBits :: BitArray -> Maybe (Bool, BitArray)
unconsBits (BitArray _ "") = Nothing
unconsBits (BitArray i bs) = do
	(b, bs') <- BS.uncons bs
	return (b `testBit` fromIntegral i, if i < 7
		then BitArray (i + 1) bs
		else BitArray 0 bs')

getNumber :: (Num a, Bits a) => Word8 -> BitArray -> Maybe (a, BitArray)
getNumber n ba = first bitsToWord <$> popBits n ba

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

uncompressAdhocFixed :: BS.ByteString -> Maybe BS.ByteString
uncompressAdhocFixed bs = do
	(_, bs') <- zlibHeader bs
	(_, ba) <- headerBits $ toBitArray bs'
	return $ adhocFixed ba

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

-- DYNAMIC

data DynamicHeaderPartial = DynamicHeaderPartial {
	dhpHLit :: HLit,
	dhpHDist :: HDist,
	dhpHCLen :: HCLen,
	dhpTableOfTable :: TableOfTable
	} deriving Show

dynamicHeaderPartial :: BitArray -> Maybe (DynamicHeaderPartial, BitArray)
dynamicHeaderPartial ba0 = do
	(hl, ba1) <- hlit ba0
	(hd, ba2) <- hdist ba1
	(hc@(HCLen l), ba3) <- hclen ba2
	(tot, ba4) <- tableOfTable l ba3
	return (DynamicHeaderPartial hl hd hc tot, ba4)

data HLit = HLit Word16 deriving Show

hlit :: BitArray -> Maybe (HLit, BitArray)
hlit ba = first (HLit . (+ 257)) <$> getNumber 5 ba

data HDist = HDist Word8 deriving Show

hdist :: BitArray -> Maybe (HDist, BitArray)
hdist ba = first (HDist . (+ 1)) <$> getNumber 5 ba

data HCLen = HCLen Word8 deriving Show

hclen :: BitArray -> Maybe (HCLen, BitArray)
hclen ba = first (HCLen . (+ 4)) <$> getNumber 4 ba

data TOTContents = TOTContents Word8 deriving Show
data TableOfTable = TableOfTable [Word8]

instance Show TableOfTable where
	show (TableOfTable ws) = intercalate ", " . map
		(\(s, w) -> show s ++ ": " ++ show w)
		$ zip [
			16 :: Word8, 17, 18, 0, 8, 7, 9, 6,
			10, 5, 11, 4, 12, 3, 13, 2,
			14, 1, 15 ] ws

pushTOTContents :: TOTContents -> TableOfTable -> TableOfTable
pushTOTContents (TOTContents tot) (TableOfTable ws) =
	TableOfTable $ tot : ws

totContents :: BitArray -> Maybe (TOTContents, BitArray)
totContents ba = first TOTContents <$> getNumber 3 ba

tableOfTable :: Word8 -> BitArray -> Maybe (TableOfTable, BitArray)
tableOfTable 0 ba = Just (TableOfTable [], ba)
tableOfTable n ba = do
	(c, ba') <- totContents ba
	(cs, ba'') <- tableOfTable (n - 1) ba'
	return (c `pushTOTContents` cs, ba'')

totPairs :: TableOfTable -> [(Word8, Word8)]
totPairs (TableOfTable ws) = zip
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
	ws

adhocGetTot :: BS.ByteString -> [(Word8, Word8)]
adhocGetTot = fromJust
	. (totPairs . dhpTableOfTable . fst <$>)
	. dynamicHeaderPartial . adhocDropHeaders

adhocGetHLit :: BS.ByteString -> HLit
adhocGetHLit = fromJust
	. (dhpHLit . fst <$>)
	. dynamicHeaderPartial . adhocDropHeaders

adhocDropDynamicHeaders :: BS.ByteString -> BitArray
adhocDropDynamicHeaders = fromJust
	. (snd <$>)
	. dynamicHeaderPartial . adhocDropHeaders

sortTable :: [(Word8, Word8)] -> [(Word8, Word8)]
sortTable = dropWhile ((== 0) . snd) . sortBy (compare `on` snd)

adhocTable :: [(Word8, Bs)]
adhocTable = [
	(0, "00"), (6, "01"), (5, "100"), (7, "101"),
	(3, "1100"), (4, "1101"), (17, "1110"),
	(1, "11110"), (18, "11111") ]

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

adhocTableTree :: Tree Word8
adhocTableTree = Node
	(Node (Leaf 0) (Leaf 6))
	(Node
		(Node (Leaf 5) (Leaf 7))
		(Node
			(Node (Leaf 3) (Leaf 4))
			(Node
				(Leaf 17)
				(Node (Leaf 1) (Leaf 18)))))

expandTable1 ::
	Word8 -> Tree Word8 -> BitArray -> Maybe ([Word8], BitArray)
expandTable1 p (Leaf w) ba = case w of
		16 -> do
			(n, ba'') <- getNumber 2 ba
			return (replicate (n + 3) p, ba'')
		17 -> do
			(n, ba'') <- getNumber 3 ba
			return (replicate (n + 3) 0, ba'')
		18 -> do
			(n, ba'') <- getNumber 7 ba
			return (replicate (n + 11) 0, ba'')
		_ -> return ([w], ba)
expandTable1 _ _ ba | nullBitArray ba = Nothing
expandTable1 p (Node t1 t2) ba = do
	(b, ba') <- unconsBits ba
	expandTable1 p (bool t1 t2 b) ba'

expandTable ::
	Word8 -> Tree Word8 -> Word16 -> BitArray -> Maybe ([Word8], BitArray)
expandTable _ _ 0 ba = Just ([], ba)
expandTable p tr n ba = do
	(w, ba') <- expandTable1 p tr ba
	(ws, ba'') <- expandTable (last w) tr (n - fromIntegral (length w)) ba'
	return (w ++ ws, ba'')

adhocGetTables :: BS.ByteString -> ([Word8], [Word8], BitArray)
adhocGetTables bs = fromJust $ do
	let	ba = adhocDropDynamicHeaders bs
	(t1, ba') <- expandTable 0 adhocTableTree 272 ba
	(t2, ba'') <- expandTable 0 adhocTableTree 13 ba'
	return (t1, t2, ba'')

data LitLenGen = LitLenGen Word16 deriving Show

charToLitLenGen :: Char -> LitLenGen
charToLitLenGen = LitLenGen . fromIntegral . ord

showTable1 :: [Word8] -> [[(Char, Word8)]]
showTable1 = groupBy ((==) `on` snd) . sortBy (compare `on` snd)
	. filter ((/= 0) . snd) . zip ['\0' .. '\285']

adhocLitLenGenTable :: [(LitLenGen, Bs)]
adhocLitLenGenTable = map (first charToLitLenGen) [
	(' ', "000"),
	('\n', "0010"), ('e', "0011"), ('l', "0100"),
	('\t', "01010"), ('\'', "01011"), ('(', "01100"), ('>', "01101"),
	('f', "01110"), ('i', "01111"), ('n', "10000"), ('r', "10001"),
	('t', "10010"), ('u', "10011"), ('\257', "10100"), ('\258', "10101"),
	('\260', "10110"),
	('$', "101110"), (')', "101111"), ('*', "110000"), ('.', "110001"),
	('<', "110010"), ('a', "110011"), ('c', "110100"), ('h', "110101"),
	('p', "110110"), ('s', "110111"), ('\256', "111000"),
	('\259', "111001"), ('\261', "111010"), ('\271', "111011"),
	('1', "1111000"), ('2', "1111001"), ('=', "1111010"),
	('g', "1111011"), ('v', "1111100"), ('w', "1111101"),
	('\262', "1111110"), ('\265', "1111111") ]

litLenTree :: Tree Word8
litLenTree = Leaf 0

data LitLen = Lit Word8 | Len Word16 deriving Show

-- TOOLS

bits :: BS.ByteString -> String
bits = concatMap bits8 . BS.unpack

bits8 :: Word8 -> String
bits8 w = map (bool '0' '1' . (w `testBit`)) [0 .. 7]

word4s :: Word8 -> (Word8, Word8)
word4s w = (w .&. 0b1111, w `shiftR` 4)

getDeflate :: BS.ByteString -> BS.ByteString
getDeflate = BS.reverse . BS.drop 4 . BS.reverse . BS.drop 2
