module TryUnsafeCoerce where

import Data.Word
import Data.Int
import Unsafe.Coerce

int1 :: Int32
int1 = - 123

word1 :: Word32
word1 = unsafeCoerce int1

int2 :: Int64
int2 = - 123

word2 :: Word64
word2 = unsafeCoerce int2
