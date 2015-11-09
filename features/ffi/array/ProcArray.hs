{-# LANGUAGE ForeignFunctionInterface #-}

module ProcArray (procArray) where

import Control.Applicative
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array

foreign import ccall unsafe "proc_array.h proc_array" c_procArray ::
	CInt -> CInt -> Ptr CInt -> IO ()

procArray :: Int -> Int -> IO [Int]
procArray n mx = allocaArray0 mx $ \p -> do
	c_procArray (fromIntegral n) (fromIntegral mx) p
	map fromIntegral <$> peekArray0 (- 1) p
