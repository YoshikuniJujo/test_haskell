{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NoCapi where

import Foreign.Ptr
import Foreign.C.Types

import Foo

foreign import ccall "add123" c_add123 :: CInt -> CInt

foreign import ccall "return_eight" c_eight :: CInt

foreign import ccall "return_add123_" c_add123_ :: CInt -> CInt

foreign import ccall "return_foo_foo1" c_foo_foo1 :: Ptr Foo -> CInt

foreign import ccall "return_foo0" c_foo0 :: Ptr Foo
