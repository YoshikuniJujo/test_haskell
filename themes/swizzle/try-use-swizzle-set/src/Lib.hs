{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.SwizzleSet qualified as SwzS
import Data.SwizzleSet.TH

type TupInt10 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

nums :: TupInt10
nums = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)


foo :: TupInt10
foo = SwzS.ywv (100, 200, 300) nums -- (0, 100, 2, 200, 300, 5, 6, 7, 8)

foo2, foo3, foo4 :: TupInt10
foo2 = SwzS.z 123 nums -- (0, 1, 123, 3, 4, 5, 6, 7, 8, 9)
foo3 = SwzS.u 321 nums -- (0, 1, 2, 3, 4, 321, 6, 7, 8, 9)
foo4 = SwzS.q 333 nums -- (0, 1, 2, 3, 4, 5, 6, 7, 8, 333)

swizzleSet "" "zvusq"

bar :: TupInt10
bar = zvusq (100, 200, 300, 400, 500) nums -- (0, 1, 100, 3, 200, 300, 6, 400, 8, 500)
