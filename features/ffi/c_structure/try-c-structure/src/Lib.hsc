{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
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

foo' :: Foo -> (CInt, CInt)
foo' (Foo_ ff) = unsafePerformIO $ withForeignPtr ff \pf -> do
	f0 <- #{peek Foo, x} pf
	f1 <- #{peek Foo, y} pf
	pure (f0, f1)

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)
