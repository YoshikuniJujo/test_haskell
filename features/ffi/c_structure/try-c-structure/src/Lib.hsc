{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH
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

(: []) <$> mkNewtype "Foo"

mkPatternFun "Foo" [
	(''CInt, [e| #{peek Foo, x} |]),
	(''CInt, [e| #{peek Foo, y} |]) ]

instance Ix CInt where
	range (i, j) = [i .. j]
	index (i, _j) i' = fromIntegral $ i' - i
	inRange (i, j) i' = i <= i' && i' <= j

(\p s b -> [p, s, b])
	<$> pure (PragmaD $ CompleteP [mkName "Foo"] Nothing)
	<*> mkPatternSig "Foo" [''CInt, ''CInt]
	<*> mkPatternBody "Foo" #{size Foo} ["x", "y"] [[e| #{poke Foo, x} |], [e| #{poke Foo, y} |]]

sequence [
	mkInstanceShow "Foo" ["x", "y"],
	mkInstanceRead "Foo" ["x", "y"],
	mkInstanceEq "Foo" ["x", "y"],
	mkInstanceOrd "Foo" ["x", "y"],
	mkInstanceBounded "Foo" ["x", "y"] ]

(: []) <$> mkIxRange (mkName "rangeFoo") "Foo" ["x", "y"]

instance Ix Foo where
	range = rangeFoo
	index (Foo x y, Foo x' y') (Foo i j) =
		index (x, x') i * rangeSize (x, x') + index (y, y') j
	inRange (Foo x y, Foo x' y') (Foo i j) =
		inRange (x, x') i && inRange (y, y') j


(: []) <$> mkNewtypePrim "Foo" [''Show]

foreign import ccall "foo_copy" c_foo_freeze :: Ptr (FooPrim s) -> IO (Ptr Foo)
foreign import ccall "foo_copy" c_foo_thaw :: Ptr Foo -> IO (Ptr (FooPrim s))
foreign import ccall "foo_copy" c_foo_copy :: Ptr (FooPrim s) -> IO (Ptr (FooPrim s))
foreign import ccall "foo_free" c_foo_free :: Ptr Foo -> IO ()
foreign import ccall "foo_free" c_foo_prim_free :: Ptr (FooPrim s) -> IO ()

(\s f -> [s, f])
	<$> mkFreezeSig "Foo"
	<*> mkFreezeFun "Foo" 'c_foo_freeze 'c_foo_free

(\s b -> [s, b])
	<$> mkThawSig "Foo"
	<*> mkThawFun "Foo" 'c_foo_thaw 'c_foo_prim_free

(\s b -> [s, b])
	<$> mkCopySig "Foo"
	<*> mkCopyFun "Foo" 'c_foo_copy 'c_foo_prim_free

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

fooScale :: PrimMonad m => FooPrim (PrimState m) -> CInt -> m ()
fooScale (FooPrim ff) s = unsafeIOToPrim $ withForeignPtr ff (`c_foo_scale` s)

foreign import ccall "foo_scale" c_foo_scale :: Ptr (FooPrim s) -> CInt -> IO ()
