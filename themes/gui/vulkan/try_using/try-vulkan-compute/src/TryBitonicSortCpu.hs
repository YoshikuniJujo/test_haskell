{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicSortCpu (bitonicSortCpu') where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Data.Foldable
import Data.Array
import Data.Word

import Data.List qualified as L

foreign import ccall "bitonic_sort" c_bitonic_sort :: CInt -> Ptr Word32 -> IO ()

bitonicSortCpu' :: CInt -> [Word32] -> IO (Array Int Word32)
bitonicSortCpu' n ns = allocaArray l \a -> do
	pokeArray a $ toList ns
	c_bitonic_sort n a
	listArray (0, l - 1) <$> peekArray l a
	where l = L.length ns
