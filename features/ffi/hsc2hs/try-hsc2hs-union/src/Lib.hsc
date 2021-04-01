module Lib where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

#include "hello.h"

foreign import ccall "hello" c_hello :: IO ()

fooSize :: Int
fooSize = #{size foo}

newtype Foo = Foo (Ptr Foo) deriving Show

foreign import ccall "get_sample_foo" c_get_sample_foo :: Ptr Foo

peekInt0, peekInt1 :: Ptr Foo -> IO CInt
peekInt0 = #{peek foo, integers.int0}
peekInt1 = #{peek foo, integers.int1}
