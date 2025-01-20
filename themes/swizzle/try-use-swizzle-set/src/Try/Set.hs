{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Set where

import Data.SwizzleSet.TH

swizzleSet "set" "ywuq"

sample :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
sample = setYwuq (100, 200, 300, 400) (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
