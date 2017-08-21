{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Word
import Data.Char
import System.IO
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import qualified Data.ByteString as BS

data Header = Header {
	name :: FilePath,
	mode :: BS.ByteString,
	uid :: BS.ByteString,
	gid :: BS.ByteString,
	size :: BS.ByteString,
	mtime :: BS.ByteString
	} deriving Show

instance Storable Header where
	sizeOf _ = 512
	alignment _ = 8
	peek p = do
		nm <- nullTermString 0 100 p
		md <- nullTermByteString 100 8 p
		ui <- nullTermByteString 108 8 p
		gi <- nullTermByteString 116 8 p
		sz <- nullTermByteString 124 12 p
		mt <- nullTermByteString 136 12 p
		return Header {
			name = nm,
			mode = md,
			uid = ui,
			gid = gi,
			size = sz,
			mtime = mt
			}

nullTermBytes :: Word8 -> Word8 -> Ptr a -> IO [Word8]
nullTermBytes os mx = ntb mx . (`plusPtr` fromIntegral os)
	where
	ntb mx _ | mx < 1 = return []
	ntb mx p = do
		w <- peek (castPtr p :: Ptr Word8)
		if (w == 0)
			then return []
			else (w :) <$> ntb (mx - 1) (p `plusPtr` 1)

nullTermString :: Word8 -> Word8 -> Ptr a -> IO String
nullTermString os = ((map (chr . fromIntegral) <$>) .) . nullTermBytes os

nullTermByteString :: Word8 -> Word8 -> Ptr a -> IO BS.ByteString
nullTermByteString os = ((BS.pack <$>) .) . nullTermBytes os
