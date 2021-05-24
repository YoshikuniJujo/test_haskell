{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Foreign.C.Struct (struct, structPrim)
import Control.Monad.Primitive (PrimMonad(..), unsafeIOToPrim)
import System.IO.Unsafe (unsafePerformIO)

#include "foo.h"

---------------------------------------------------------------------------

-- * DEFINITION
-- * SAMPLE

---------------------------------------------------------------------------
-- DEFINITION
---------------------------------------------------------------------------

struct "Foo" #{size Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded]

foreign import ccall "foo_copy" c_foo_copy :: Ptr Foo -> IO (Ptr Foo)
foreign import ccall "foo_free" c_foo_free :: Ptr Foo -> IO ()

structPrim "Foo" 'c_foo_copy 'c_foo_free [''Show]

---------------------------------------------------------------------------
-- SAMPLE
---------------------------------------------------------------------------

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> sampleFooGen

sampleFooPrim :: PrimMonad m => m (FooPrim (PrimState m))
sampleFooPrim = unsafeIOToPrim $ FooPrim <$> sampleFooGen

sampleFooGen :: IO (ForeignPtr Foo)
sampleFooGen = (`newForeignPtr` pure ()) =<< c_sample_foo

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)

fooScale :: PrimMonad m => FooPrim (PrimState m) -> CInt -> m ()
fooScale (FooPrim ff) s = unsafeIOToPrim $ withForeignPtr ff (`c_foo_scale` s)

foreign import ccall "foo_scale" c_foo_scale :: Ptr Foo -> CInt -> IO ()
