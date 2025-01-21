{-# LANGUAGE ImportQualifiedPost #-}

module Lib where

import Data.SwizzleModify qualified as SwzM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Tup10 a = (a, a, a, a, a, a, a, a, a, a)

nums :: Tup10 Int
nums = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

foo = SwzM.ywu ((* 100), (* 200), (*300)) nums
