module Main (main) where

import Lib

main :: IO ()
main = cxx_main_cxx

foreign import ccall "main_cxx" cxx_main_cxx :: IO ()
