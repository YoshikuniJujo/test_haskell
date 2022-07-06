{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

main :: IO ()
main = getArgs >>= \case
	[x, y, z] -> putStrLn $ make x y z
	_ -> error "stack runghc genjitsutouhi.hs foo bar baz"

make :: String -> String -> String -> String
make x y z =
	"婚活の初デートで" ++ x ++
	"が好きか?と毎回相手に聞く女性がいて不思議だったので" ++
	"理由を聞いてみたら「あれは現実逃避した人が好む" ++ y ++
	"だから、仕事がうまく行っている男性はあんなものは" ++ z ++
	"ない」と教えてくれて目からウロコでした。"
