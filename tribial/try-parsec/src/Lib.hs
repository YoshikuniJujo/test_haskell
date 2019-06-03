module Lib
    ( someFunc
    ) where

import Data.Char
import Text.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

num :: Stream s m Char => ParsecT s u m Int
num = digitToInt <$> digit
