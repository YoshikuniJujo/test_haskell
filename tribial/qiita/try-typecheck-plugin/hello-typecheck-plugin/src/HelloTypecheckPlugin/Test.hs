{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=HelloTypecheckPlugin.Plugin #-}

module HelloTypecheckPlugin.Test where

some :: Int -> Bool
some = id
