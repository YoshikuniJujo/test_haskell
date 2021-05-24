{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Control.Monad.Primitive (PrimMonad(..), unsafeIOToPrim)
import Data.Array (Ix(..))
import System.IO.Unsafe (unsafePerformIO)

import Foreign.C.Struct (struct, structPrim)

#include "foo.h"

---------------------------------------------------------------------------

-- * DEFINITION
-- * SAMPLE
-- * INSTANCE IX CINT

---------------------------------------------------------------------------
-- DEFINITION
---------------------------------------------------------------------------

struct "Foo" #{size Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded, ''Ix]

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

---------------------------------------------------------------------------
-- INSTANCE IX CINT
---------------------------------------------------------------------------

instance Ix CInt where
	range (l, u) = [l .. u]
	index (l, _) i = fromIntegral $ i - l
	inRange (l, u) i = l <= i && i <= u
