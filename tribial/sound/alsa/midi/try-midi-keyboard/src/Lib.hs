{-# LANGUAGE BlockArguments #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Function
import Data.Word
import System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"

hGetBytes :: Handle -> IO [Word8]
hGetBytes h = allocaBytes 3 \p -> do
	n <- hGetBuf h p 3
	peekArray n p

foo h = fix \go -> hGetBytes h >>= print >> go
