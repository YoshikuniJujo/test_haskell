{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Base
import Control.Monad.Primitive

foreign import ccall "slozsoft" c_slozsoft_gen :: IO ()

c_slozsoft_ :: State# s -> (# State# s, () #)
c_slozsoft_ = unsafeCoerce# (unIO c_slozsoft_gen)

c_slozsoft :: PrimMonad m => m ()
c_slozsoft = primitive c_slozsoft_

foreign import ccall "hello" c_hello_gen :: IO ()

unPrimIo :: PrimMonad m => IO a -> m a
unPrimIo = primitive . unsafeCoerce# . unIO

c_hello :: PrimMonad m => m ()
c_hello = unPrimIo c_hello_gen

main :: IO ()
main = do
	c_slozsoft >> c_slozsoft
	c_hello >> c_hello
