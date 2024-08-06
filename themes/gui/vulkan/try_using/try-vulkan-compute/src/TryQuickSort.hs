{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryQuickSort (quicksort) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Data.Maybe
import Data.List qualified as L
import Data.Word

foreign import ccall "quicksort" c_quicksort :: CInt -> CInt -> Ptr Word32 -> IO ()

quicksort :: CInt -> [Word32] -> IO [Word32]
quicksort m ns = allocaArray (l + 2) \a -> do
	pokeArray a (minBound : ns ++ [maxBound])
	c_quicksort m (fromIntegral l) a
	tail' <$> peekArray (l + 1) a
	where l = L.length ns

tail' :: [a] -> [a]
tail' = snd . fromJust . L.uncons
