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
import Text.Read

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

(\s b -> [s, b])
	<$> mkPatternSig "Foo" [''CInt, ''CInt]
	<*> mkPatternBody "Foo" #{size Foo} ["x", "y"] [[e| #{poke Foo, x} |], [e| #{poke Foo, y} |]]

(: []) <$> mkInstanceShow "Foo" ["x", "y"]

instance Read Foo where
	readPrec = parens $ prec appPrec do
		Ident "Foo" <- lexP
		Punc "{" <- lexP
		Ident "fooX" <- lexP
		Punc "=" <- lexP
		x <- step readPrec
		Punc "," <- lexP
		Ident "fooY" <- lexP
		Punc "=" <- lexP
		y <- step readPrec
		Punc "}" <- lexP
		pure $ Foo x y
		where appPrec = 10

sampleFoo :: Foo
sampleFoo = unsafePerformIO $ Foo_ <$> do
	p <- c_sample_foo
	newForeignPtr p $ pure ()

foreign import ccall "sample_foo" c_sample_foo :: IO (Ptr Foo)
