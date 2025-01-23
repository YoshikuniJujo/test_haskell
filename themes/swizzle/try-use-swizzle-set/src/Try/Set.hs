{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Set where

import Data.SwizzleSet.TH

swizzleSet "set" "ywuq"

sample :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
sample = setYwuq (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) (100, 200, 300, 400)
	-- (0, 100, 2, 200, 4, 300, 6, 7, 8, 400)
