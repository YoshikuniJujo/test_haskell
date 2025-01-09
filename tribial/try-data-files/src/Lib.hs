module Lib
    ( someFunc
    ) where

import Paths_try_data_files

someFunc :: IO ()
someFunc = getDataFileName "data/foo.txt" >>= readFile >>= putStr
