{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.SwizzleModify qualified as SwzM
import Data.SwizzleModify.TH

nums :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
nums = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

foo :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
foo = SwzM.ywv ((* 100), (* 200), (* 300)) nums -- (0, 100, 2, 600, 1200, 5, 6, 7, 8, 9)

bar :: (Int, Int, String, String, Int, Int, Int, Int, Int, Int)
bar = SwzM.zw (show, show) nums -- (0, 1, "2", "3", 4, 5, 6, 7, 8, 9)

swizzleModify "" "ztsq"

baz :: (Int, Int, String, Int, Int, Int, Int, String, Int, Char)
baz = ztsq (show, (* 100), (`replicate` 'c'), const 'q') nums
	-- (0, 1, "2", 3, 4, 5, 600, "ccccccc", 8, 'q')
