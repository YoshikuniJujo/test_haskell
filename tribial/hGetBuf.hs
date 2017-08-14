{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Fix
import Data.Word
import Data.Int
import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

intSize :: Int
intSize = sizeOf (0 :: Int)

hReadInt :: Handle -> IO Int
hReadInt h = alloca $ \p -> do
	n <- hGetBuf h p intSize
	if n /= intSize then error "error occur" else peek p

word8Size :: Int
word8Size = sizeOf (0 :: Word8)

hReadWord8 :: Handle -> IO Word8
hReadWord8 h = alloca $ \p -> do
	n <- hGetBuf h p word8Size
	if n /= word8Size then error "error occur" else peek p

hWriteWord8 :: Handle -> Word8 -> IO ()
hWriteWord8 h w = alloca $ \p -> do
	poke p w
	hPutBuf h p word8Size

word32Size :: Int
word32Size = sizeOf (0 :: Word32)

hReadWord32 :: Handle -> IO Word32
hReadWord32 h = alloca $ \p -> do
	n <- hGetBuf h p word32Size
	if n /= word32Size then error "error occur" else peek p

hReadValue :: forall a . Storable a => Handle -> IO a
hReadValue h = alloca $ \p -> do
	let s = sizeOf (undefined :: a)
	n <- hGetBuf h p s
	if n /= s then error "error occur" else peek p

hReadValue' :: Storable a => Handle -> IO a
hReadValue' h = do
	let t = undefined :: Storable a => a
	alloca $ \p -> do
		n <- hGetBuf h p $ sizeOf t
		if n /= sizeOf t then error "error occur" else do
			r <- peek p
			return $ r `asTypeOf` t

hReadValueMaybe :: forall a . Storable a => Handle -> IO (Maybe a)
hReadValueMaybe h = alloca $ \p -> do
	n <- hGetBuf h p $ sizeOf (undefined :: a)
	if n /= sizeOf (undefined :: a) then return Nothing else Just <$> peek p

hReadValues :: Storable a => Handle -> IO [a]
hReadValues h = do
	mx <- hReadValueMaybe h
	case mx of
		Just x -> (x :) <$> hReadValues h
		Nothing -> return []

hWriteValue :: forall a . Storable a => Handle -> a -> IO ()
hWriteValue h x = alloca $ \p ->  do
	poke p x
	hPutBuf h p $ sizeOf x

newtype Word64BE = Word64BE Word64 deriving Show
newtype Word64LE = Word64LE Word64 deriving Show

buildLittleEndian :: Num a => [Word8] -> a
buildLittleEndian [] = 0
buildLittleEndian (w : ws) = fromIntegral w + 2 ^ 8 * buildLittleEndian ws

destroyLittleEndian :: Integral a => a -> [Word8]
destroyLittleEndian n | n <= 0 = []
destroyLittleEndian n = let (d, m) = n `divMod` (2 ^ 8) in
	fromIntegral m : destroyLittleEndian d

instance Storable Word64LE where
	sizeOf _ = 8
	alignment _ = 8
	peek p = Word64LE . buildLittleEndian <$> peekArray 8 (castPtr p)
	poke p (Word64LE w) = pokeArray (castPtr p) $ destroyLittleEndian w

buildBigEndian :: Num a => [Word8] -> a
buildBigEndian = bbe 0
	where
	bbe n [] = n
	bbe n (w : ws) = bbe (n * 2 ^ 8 + fromIntegral w) ws

destroyBigEndian :: Integral a => a -> [Word8]
destroyBigEndian = dbe []
	where
	dbe ws n | n <= 0 = ws
	dbe ws n = let (d, m) = n `divMod` (2 ^ 8) in
		dbe (fromIntegral m : ws) d

instance Storable Word64BE where
	sizeOf _ = 8
	alignment _ = 8
	peek p = Word64BE . buildBigEndian <$> peekArray 8 (castPtr p)
	poke p (Word64BE n) = pokeArray (castPtr p) $ destroyBigEndian n

data Shape = Rectangle {
	leftEdge :: Int32,
	topEdge :: Int32,
	width :: Int32,
	height :: Int32
	} deriving Show

peekRectangle :: Ptr Int32 -> IO Shape
peekRectangle p = do
	[l, t, w, h] <- peekArray 4 p
	return Rectangle {
		leftEdge = l,
		topEdge = t,
		width = w,
		height = h }

pokeShape :: Ptr Int32 -> Shape -> IO ()
pokeShape p r@(Rectangle { }) = do
	poke (castPtr p) (1 :: Word8)
	pokeArray (p `plusPtr` 1) $
		map ($ r) [leftEdge, topEdge, width, height]

instance Storable Shape where
	sizeOf _ = 17
	alignment _ = 8
	peek p = do
		k <- peek (castPtr p) :: IO Word8
		case k of
			1 -> peekRectangle (castPtr $ p `plusPtr` 1)
			_ -> error "no such shape"
	poke = pokeShape . castPtr
