{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import System.IO

data Suit = Spade | Hart | Diamond | Club deriving (Show, Enum)

instance Storable Suit where
	sizeOf _ = 1
	alignment _ = 1
	peek p = toEnum . fromIntegral <$> peek (castPtr p :: Ptr Word8)
	poke p = poke (castPtr p :: Ptr Word8) . fromIntegral . fromEnum

hReadStorable :: forall a . Storable a => Handle -> IO (Maybe a)
hReadStorable h = alloca $ \p -> do
	let	t = undefined :: a
	n <- hGetBuf h p $ sizeOf t
	if n == sizeOf t
		then Just <$> peek p
		else return Nothing

hReadStorable' :: Storable a => Handle -> IO (Maybe a)
hReadStorable' h = alloca $ \p -> do
	let	t = undefined :: Storable a => a
	n <- hGetBuf h p $ sizeOf t
	if n == sizeOf t
		then do	r <- peek p
			return . Just $ r `asTypeOf` t
		else return Nothing

hWriteStorable :: Storable a => Handle -> a -> IO ()
hWriteStorable h x = alloca $ \p -> do
	poke p x
	hPutBuf h p $ sizeOf x
