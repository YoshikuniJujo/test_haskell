{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Word
import Data.Char
import System.IO
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI

type PtrIO = StateT (Ptr Word8) IO

data Header = Header {
	name :: FilePath,
	mode :: BS.ByteString,
	uid :: BS.ByteString,
	gid :: BS.ByteString,
	size :: BS.ByteString,
	mtime :: BS.ByteString,
	chksum :: BS.ByteString,
	typeflag :: BS.ByteString,
	linkname :: FilePath,
	magic :: BS.ByteString,
	uname :: BS.ByteString,
	gname :: BS.ByteString,
	devmajor :: BS.ByteString,
	devminor :: BS.ByteString,
	prefix :: FilePath
	} deriving Show

instance Storable Header where
	sizeOf _ = 512
	alignment _ = 8
	peek = evalStateT peekHeader . castPtr

peekHeader :: PtrIO Header
peekHeader = do
	p <- get
	h <- Header
		<$> (nullTermString <$> readByteString 100)
		<*> readByteString 8
		<*> readByteString 8
		<*> readByteString 8
		<*> readByteString 12
		<*> readByteString 12
		<*> readByteString 8
		<*> readByteString 1
		<*> (nullTermString <$> readByteString 100)
		<*> readByteString 8
		<*> (BS.takeWhile (/= 0) <$> readByteString 32)
		<*> (BS.takeWhile (/= 0) <$> readByteString 32)
		<*> (BS.takeWhile (/= 0) <$> readByteString 8)
		<*> (BS.takeWhile (/= 0) <$> readByteString 8)
		<*> (nullTermString <$> readByteString 155)
	lift $ do
		pokeArray (p `plusPtr` 148) $ replicate 8 (32 :: Word8)
		sumBytes 512 p >>= print
	return h

nullTermString :: BS.ByteString -> String
nullTermString = BSC.unpack . BSC.takeWhile (/= '\0')

readByteString :: Word8 -> PtrIO BS.ByteString
readByteString n_ = get >>= \p -> BS.takeWhile (/= 0)
	<$> lift (BSI.create n $ \b -> copyBytes b p n) <* put (p `plusPtr` n)
	where n = fromIntegral n_

sumBytes :: Word16 -> Ptr a -> IO Word32
sumBytes n p = sum . map fromIntegral
	<$> peekArray (fromIntegral n) (castPtr p :: Ptr Word8)
