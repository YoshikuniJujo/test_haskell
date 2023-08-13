{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryQuickSort where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Data.Foldable
import Data.Array
import Data.Word

data Stack = Stack (Ptr Stack) deriving Show

foreign import ccall "empty" c_empty :: IO (Ptr Stack)

empty :: IO Stack
empty = Stack <$> c_empty

foreign import ccall "push" c_push :: CInt -> CInt -> Ptr Stack -> IO ()

push :: CInt -> CInt -> Stack -> IO ()
push p q (Stack s) = c_push p q s

foreign import ccall "pop" c_pop :: Ptr CInt -> Ptr CInt -> Ptr Stack -> IO CInt

pop :: Stack -> IO (Maybe (CInt, CInt))
pop (Stack s) = alloca \p -> alloca \q -> do
	r <- c_pop p q s
	if r /= 0 then (Just .) . (,) <$> peek p <*> peek q else pure Nothing

foreign import ccall "quicksort" c_quicksort :: CInt -> CInt -> Ptr Word32 -> IO ()

quicksort :: CInt -> Array Int Word32 -> IO (Array Int Word32)
quicksort m ns = allocaArray (length ns + 2) \a -> do
	pokeArray a (minBound : toList ns ++ [maxBound])
	c_quicksort m (fromIntegral $ length ns) a
	listArray (0, length ns - 1) . tail <$> peekArray (length ns + 1) a
