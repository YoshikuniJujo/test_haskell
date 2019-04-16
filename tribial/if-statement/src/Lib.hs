module Lib
    ( someFunc
    ) where

import Data.Default

someFunc :: IO ()
someFunc = putStrLn "someFunc"

ifs :: Default a => Bool -> a -> a
ifs True x = x
ifs False _ = def
