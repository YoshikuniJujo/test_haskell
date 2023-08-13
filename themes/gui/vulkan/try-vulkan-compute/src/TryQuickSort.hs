{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryQuickSort where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Types

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
