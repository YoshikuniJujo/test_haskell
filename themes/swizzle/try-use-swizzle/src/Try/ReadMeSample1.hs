{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Try.ReadMeSample1 where

import Data.Tuple
import Data.Swizzle qualified as Swz
import Data.Swizzle.TH

nums :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
nums = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

foo :: (Int, Int, Int, Int)
foo = Swz.zyxw nums -- (3, 2, 1, 0)

foo2, foo3, foo4, foo5, foo9 :: Solo Int
foo2 = Swz.z nums -- 2
foo3 = Swz.w nums -- 3
foo4 = Swz.v nums -- 4
foo5 = Swz.u nums -- 5
foo9 = Swz.q nums -- 9

swizzle "wyvyuqztuwurqsq"

bar = wyvyuqztuwurqsq (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	-- (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9)
