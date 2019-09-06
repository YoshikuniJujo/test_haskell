{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Lib

main :: IO ()
main = do
	fp <- fakePrefectureIO
	putStrLn $ "あなたの出身は、" ++ fp ++ "です。"
