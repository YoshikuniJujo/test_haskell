{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicSortPairs (
	trySeparateNum, bitSeparate ) where

import Foreign.C.Types

foreign import ccall "try_separate_num" c_try_separate_num :: CInt -> IO ()

trySeparateNum :: Int -> IO ()
trySeparateNum = c_try_separate_num . fromIntegral

foreign import ccall "bit_separate" c_bit_separate :: CInt -> CInt -> CInt

bitSeparate :: Int -> Int -> Int
bitSeparate bs i =
	fromIntegral $ c_bit_separate (fromIntegral bs) (fromIntegral i)

foreign import ccall "check_bit" c_check_bit :: CInt -> CInt -> CInt

checkBit :: Int -> Int -> Bool
checkBit bs i = c_check_bit (fromIntegral bs) (fromIntegral i) /= 0
