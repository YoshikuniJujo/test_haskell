{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Lifegame.Glider where

shape0, shape1, shape2, shape3 :: [String]
shape0 = ["*..", ".**", "**."]
shape1 = [".*.", "..*", "***"]
shape2 = ["*.*", ".**", ".*."]
shape3 = ["..*", "*.*", ".**"]

printShape :: [String] -> IO ()
printShape = (putStrLn `mapM_`)
