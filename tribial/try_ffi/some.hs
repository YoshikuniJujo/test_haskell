{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Base
import Control.Monad.Primitive

foreign import ccall "slozsoft" c_slozsoft_gen :: IO ()

c_slozsoft :: State# s -> (# State# s, () #)
c_slozsoft = unsafeCoerce# (unIO c_slozsoft_gen)

main :: IO ()
main = do
	primitive c_slozsoft
	primitive c_slozsoft
