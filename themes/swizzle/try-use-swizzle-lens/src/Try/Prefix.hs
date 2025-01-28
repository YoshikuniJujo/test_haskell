{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Prefix where

import Control.Lens
import Data.SwizzleLens.TH

swizzleLens "lens" "wtsq"

foo :: (Int, Int, Int, Int)
foo = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) ^. lensWtsq	-- (3, 6, 7, 9)

bar :: (Int, Int, Int, Char, Int, Int, Bool, String, Int, ())
bar = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) & lensWtsq .~ ('c', True, "hello", ())
	-- (0, 1, 2, 'c', 4, 5, True, "hello", 8, ())
