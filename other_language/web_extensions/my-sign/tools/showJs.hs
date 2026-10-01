module Main where

import System.FilePath
import TsFiles

main :: IO ()
main = mapM_ (putStrLn . takeFileName) . snd =<< tsJsFiles
