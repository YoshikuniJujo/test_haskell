{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "foo" c_foo :: Ptr CInt -> IO ()
foreign import ccall "foo_array" c_foo_array :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foo :: CInt -> IO CInt
foo n = alloca \p -> do
	poke p n
	c_foo p
	peek p

foo_array :: [String] -> IO [String]
foo_array ss = allocaArray (length ss) \p -> do
	css <- newCString `mapM` ss
	pokeArray p css
	(n', p') <- alloca \pn -> do
		poke pn . fromIntegral $ length ss
		p' <- alloca \pp -> do
			poke pp p
			c_foo_array pn pp
			peek pp
		(, p') <$> peek pn
	(peekCString `mapM`) =<< peekArray (fromIntegral n') p'
