{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Lens
import Data.SwizzleLens qualified as SwzL
import Data.SwizzleLens.TH

foo :: (Int, Int, Int)
foo = (0, 1, 2, 3, 4, 5, 6, 7, 8) ^. SwzL.ywv	-- (1, 3, 4)

bar :: (Int, Int, Char, Int, String, Int, Int, Int, Int)
bar = (0, 1, 2, 3, 4, 5, 6, 7, 8) & SwzL.zv .~ ('c', "hello")
	-- (0, 1, 'c', 3, "hello", 5, 6, 7, 8)

swizzleLens "" "ywtr"

baz :: (Int, Int, Int, Int)
baz = (0, 1, 2, 3, 4, 5, 6, 7, 8) ^. ywtr	-- (1, 3, 6, 8)

qux :: (Int, Bool, Int, String, Int, Int, Char, Int, ())
qux = (0, 1, 2, 3, 4, 5, 6, 7, 8) & ywtr .~ (False, "hello", 'c', ())
	-- (0, False, 2, "hello", 4, 5, 'c', 7, ())
