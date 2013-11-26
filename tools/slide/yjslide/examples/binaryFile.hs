import Foreign.Marshal
import System.IO
import Data.Word

getBytes :: Handle -> Int -> IO [Word8]
getBytes h n = allocaBytes n $ \p -> do
	s <- hGetBuf h p n
	peekArray s p

getBytesSome :: Handle -> Int -> IO [Word8]
getBytesSome h n = allocaBytes n $ \p -> do
	s <- hGetBufSome h p n
	peekArray s p

getBytesNonBlocking :: Handle -> Int -> IO [Word8]
getBytesNonBlocking h n = allocaBytes n $ \p -> do
	s <- hGetBufNonBlocking h p n
	peekArray s p

putBytes :: Handle -> [Word8] -> IO ()
putBytes h ws = allocaBytes (length ws) $ \p -> do
	pokeArray p ws
	hPutBuf h p (length ws)
