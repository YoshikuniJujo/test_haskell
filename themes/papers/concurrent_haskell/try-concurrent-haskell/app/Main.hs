module Main where

import System.IO

echo :: IO Char
echo = hGetChar stdin >>= \c -> hPutChar stdout c >> return c

main :: IO ()
main = echo >>= \c -> if c == '\n' then return () else main
