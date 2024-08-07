{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryQuicksort (quicksort) where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.C.Types (CInt(..))
import Data.List qualified as L
import Data.Word (Word32)
import Tools (tail')

foreign import ccall "quicksort"
	c_quicksort :: CInt -> CInt -> Ptr Word32 -> IO ()

quicksort :: Int -> [Word32] -> IO [Word32]
quicksort m ns = allocaArray (l + 2) \a -> do
	pokeArray a $ minBound : ns ++ [maxBound]
	c_quicksort (fromIntegral m) (fromIntegral l) a
	tail' <$> peekArray (l + 1) a
	where l = L.length ns
