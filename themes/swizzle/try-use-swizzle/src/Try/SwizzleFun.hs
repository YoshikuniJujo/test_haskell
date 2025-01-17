{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleFun where

import Data.Swizzle qualified as Swz
import Data.Swizzle.TH

nums = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

foo = Swz.wzyx nums

foo2 = Swz.z nums
foo3 = Swz.w nums
foo4 = Swz.v nums
foo5 = Swz.u nums
foo9 = Swz.q nums

swizzle "" "wyvyuqztuwurqsq"

bar = wyvyuqztuwurqsq (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
