{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
-- import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import System.IO.Unsafe

import Template

#include "foo.h"

(: []) <$> mkNewtype "Foo"

mkPatternFun "Foo" [
	(''CInt, [e| #{peek Foo, x} |]),
	(''CInt, [e| #{peek Foo, y} |]) ]

(\s b -> [s, b])
	<$> mkPatternSig "Foo" [''CInt, ''CInt]
	<*> mkPatternBody "Foo" #{size Foo} ["x", "y"] [[e| #{poke Foo, x} |], [e| #{poke Foo, y} |]]

(: []) <$> mkInstanceShow "Foo" ["x", "y"]
(: []) <$> mkInstanceRead "Foo" ["x", "y"]

(: []) <$> mkNewtypePrim "Foo" [''Show]

foreign import ccall "foo_copy" c_foo_freeze :: Ptr (FooPrim s) -> IO (Ptr Foo)
foreign import ccall "foo_free" c_foo_free :: Ptr Foo -> IO ()

(\s f -> [s, f])
	<$> mkFreezeSig "Foo"
	<*> mkFreezeFun "Foo" 'c_foo_freeze 'c_foo_free

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

sampleFooPrim :: PrimMonad m => m (FooPrim (PrimState m))
sampleFooPrim = unsafeIOToPrim $ FooPrim <$> do
	p <- c_sample_foo_prim
	newForeignPtr p $ pure ()

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)
foreign import ccall "sample_foo" c_sample_foo_prim :: IO (Ptr (FooPrim s))
