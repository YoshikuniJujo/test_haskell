{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicsortCpu (bitonicsortCpu) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Data.Foldable
import Data.Word

import Data.List qualified as L

foreign import ccall "bitonic_sort" c_bitonic_sort :: CInt -> Ptr Word32 -> IO ()

bitonicsortCpu :: Int -> [Word32] -> IO [Word32]
bitonicsortCpu n ns = allocaArray l \a -> do
	pokeArray a $ toList ns
	c_bitonic_sort (fromIntegral n) a
	peekArray l a
	where l = L.length ns
