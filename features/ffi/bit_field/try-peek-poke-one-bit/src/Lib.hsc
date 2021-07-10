{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types

#include "foo.h"

data {-# CTYPE "foo.h" "Foo" #-} Foo

newFooAB :: CInt -> CInt -> IO (Ptr Foo)
newFooAB a b = do
	p <- mallocBytes #{size Foo}
	#{poke Foo, a} p a
	#{poke Foo, b} p b
	pure p

peekFooAB :: Ptr Foo -> IO (CInt, CInt)
peekFooAB p = (,) <$> #{peek Foo, a} p <*> #{peek Foo, b} p

foreign import capi "foo.h peek_foo_c" peekFooC :: Ptr Foo -> IO CInt

foreign import capi "foo.h poke_foo_c" pokeFooC :: Ptr Foo -> CInt -> IO ()
