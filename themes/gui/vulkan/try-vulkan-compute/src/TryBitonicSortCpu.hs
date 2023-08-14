{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicSortCpu where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Data.Foldable
import Data.Array
import Data.Word

foreign import ccall "bitonic_sort" c_bitonic_sort :: CInt -> Ptr Word32 -> IO ()

bitonicSortCpu :: CInt -> Array Int Word32 -> IO (Array Int Word32)
bitonicSortCpu n ns = allocaArray (length ns) \a -> do
	pokeArray a $ toList ns
	c_bitonic_sort n a
	listArray (0, length ns - 1) <$> peekArray (length ns) a
