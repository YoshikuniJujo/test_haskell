{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
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

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)
