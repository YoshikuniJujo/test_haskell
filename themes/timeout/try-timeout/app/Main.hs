{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal
import Control.Monad
import Control.Concurrent
import Data.Bool
import System.IO
import System.Timeout
import System.Posix

import qualified Timeout

foreign import ccall "fgets" c_fgets :: CString -> CInt -> Ptr () -> IO CString
foreign import ccall "fgets_printf" c_fgets_printf :: CString -> CInt -> Ptr () -> IO CString
foreign import ccall "fclose" c_fclose :: Ptr () -> IO CInt
foreign import ccall "fdopen" c_fdopen :: Fd -> CString -> IO (Ptr ())

fgets :: Handle -> Int -> IO (Maybe String)
fgets h n = do
	fd <- handleToFd h
	allocaBytes n $ \cs -> do
		withCString "r" $ \rd -> do
			strm <- c_fdopen fd rd
			r <- c_fgets_printf cs (fromIntegral n) strm
			bool (Just <$> peekCString cs) (pure Nothing) (r == nullPtr)
				<* c_fclose strm

main :: IO ()
main = do
	putStrLn "System.Timeout.timeout (Haskell): "
	maybe (putStrLn "timeout occur") putStrLn =<< timeout 3000000 getLine
	putStrLn "My own timeout: "
	maybe (putStrLn "my own timeout") putStrLn =<< Timeout.timeout 3000000 (() <$ hWaitForInput stdin (- 1)) (hGetLine stdin)
	putStrLn "System.Timeout.timeout (FFI): "
	maybe (putStrLn "timeout or error occur") putStrLn =<< join <$> timeout 3000000 (fgets stdin 8)
	putStrLn "sleep 10 minutes"
	threadDelay 10000000
