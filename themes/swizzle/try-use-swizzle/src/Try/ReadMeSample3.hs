{-# LANGUAGE TemplateHaskell #-}

module Try.ReadMeSample3 where

import Data.Swizzle.TH

swizzle "get" "yywu"

sample :: (Int, Int, Int, Int)
sample = getYywu (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
