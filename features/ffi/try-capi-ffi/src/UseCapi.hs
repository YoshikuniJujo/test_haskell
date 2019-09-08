{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseCapi where

import Foreign.Ptr
import Foreign.C.Types

import Foo

foreign import ccall "add123" c_add123 :: CInt -> CInt

foreign import ccall "return_foo0" c_foo0 :: Ptr Foo

foreign import capi "hello.h value eight" c_eight :: CInt

foreign import capi "hello.h add123_" c_add123_ :: CInt -> CInt

foreign import capi "hello.h foo_foo1" c_foo_foo1 :: Ptr Foo -> CInt

-- foreign import capi "hello.h foo_foo1" c_foo_foo1' :: Ptr Boo -> CInt
