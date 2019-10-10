{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=DoNothing.Plugin -fplugin-opt=DoNothing.Plugin:abc,def
	-fplugin-opt=DoNothing.Plugin:ghi #-}

module Lib2
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
