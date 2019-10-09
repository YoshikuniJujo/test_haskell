{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=DoNothing.Plugin #-}

module Lib2
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
