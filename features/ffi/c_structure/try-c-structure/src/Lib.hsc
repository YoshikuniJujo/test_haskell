{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import System.IO.Unsafe

import Template

#include "foo.h"

(: []) <$> mkNewtype "Foo"

some :: Ptr Foo -> IO CInt
some = #{peek Foo, x}

bar :: ExpQ
bar = [e| #{peek Foo, x} |]

mkPatternFun "Foo" [
	(''CInt, [e| #{peek Foo, x} |]),
	(''CInt, [e| #{peek Foo, y} |]) ]

(: []) <$> mkPatternSig "Foo" [''CInt, ''CInt]
pattern Foo { fooX, fooY } <- (foo -> (fooX, fooY)) where
	Foo x y = unsafePerformIO $ Foo_ <$> do
		p <- mallocBytes #{size Foo}
		#{poke Foo, x} p x
		#{poke Foo, y} p y
		newForeignPtr p (free p)

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)
