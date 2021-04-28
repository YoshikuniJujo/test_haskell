module Lib where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

#include "foo.h"

foo :: Integer
foo = #{const FOO}

newtype Bar = Bar (Ptr Bar) deriving Show

sampleBar :: IO Bar
sampleBar = Bar <$> c_sample_bar

foreign import ccall "sample_bar" c_sample_bar :: IO (Ptr Bar)

bee :: Bar -> IO CInt
bee (Bar p) = #{peek struct Bar, bee} p

newtype Baz = Baz (Ptr Baz) deriving Show

sampleBaz :: IO Baz
sampleBaz = Baz <$> c_sample_baz

foreign import ccall "sample_baz" c_sample_baz :: IO (Ptr Baz)

{-
b0 :: Baz -> IO CInt
b0 (Baz p) = #{peek struct Baz, b0} p
-}
