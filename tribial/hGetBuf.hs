{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Fix
import Data.Word
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
