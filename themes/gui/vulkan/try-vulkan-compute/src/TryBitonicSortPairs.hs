{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicSortPairs (trySeparateNum) where

import Foreign.C.Types

foreign import ccall "try_separate_num" c_try_separate_num :: CInt -> IO ()

trySeparateNum :: Int -> IO ()
trySeparateNum = c_try_separate_num . fromIntegral
