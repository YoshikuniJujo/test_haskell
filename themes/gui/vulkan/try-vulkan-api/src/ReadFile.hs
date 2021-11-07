{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ReadFile where

import Foreign.Marshal.Alloc
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Concurrent (newForeignPtr)

import qualified System.IO as S

readFile :: FilePath -> IO (ForeignPtr a, Int)
readFile fp = do
	h <- S.openFile fp S.ReadMode
	n <- fromIntegral <$> S.hFileSize h
	b <- mallocBytes n
	n' <- S.hGetBuf h b n
	S.hClose h
	(, n') <$> newForeignPtr b (free b)
