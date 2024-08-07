{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicsortCpu (bitonicsortCpu) where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.C.Types (CInt(..))
import Data.List qualified as L
import Data.Word (Word32)

foreign import ccall "bitonicsort" c_bitonicsort :: CInt -> Ptr Word32 -> IO ()

bitonicsortCpu :: Int -> [Word32] -> IO [Word32]
bitonicsortCpu n ns = allocaArray l \a ->
	pokeArray a ns >> c_bitonicsort (fromIntegral n) a >> peekArray l a
	where l = L.length ns
