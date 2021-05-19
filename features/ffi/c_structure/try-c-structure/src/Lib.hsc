{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Array
import System.IO.Unsafe

import Template

#include "foo.h"

struct "Foo" #{size Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded, ''Ix]

foreign import ccall "foo_copy" c_foo_copy :: Ptr Foo -> IO (Ptr Foo)
foreign import ccall "foo_free" c_foo_free :: Ptr Foo -> IO ()

structPrim "Foo" 'c_foo_copy 'c_foo_free [''Show]

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

sampleFooPrim :: PrimMonad m => m (FooPrim (PrimState m))
sampleFooPrim = unsafeIOToPrim $ FooPrim <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)

fooScale :: PrimMonad m => FooPrim (PrimState m) -> CInt -> m ()
fooScale (FooPrim ff) s = unsafeIOToPrim $ withForeignPtr ff (`c_foo_scale` s)

foreign import ccall "foo_scale" c_foo_scale :: Ptr Foo -> CInt -> IO ()

instance Ix CInt where
	range (i, j) = [i .. j]
	index (i, _j) i' = fromIntegral $ i' - i
	inRange (i, j) i' = i <= i' && i' <= j
