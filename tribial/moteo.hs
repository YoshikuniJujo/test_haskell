{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List
import System.Environment

main :: IO ()
main = do
	u : j : a : a2 : c <- getArgs
	putStrLn $ tweet c u j a a2

textA, textB, textC, textD, textE, textF, textG :: String
textA = "私が知っているモテる男の人たちに共通しているのは、"
textB = "のだいたい"
textC = "くらいを事前に"
textD = "しておき、女の子に「("
textE = "していることは言わず)どれ"
textF = "?」と聞いて、スマートに"
textG = "できるようにしておくということ。"

choices :: [String]
choices = ["フレンチ", "中華", "和食"]
unit :: String
unit = "軒"
jizen :: String
jizen = "予約"
action :: String
action = "が食べたい"
action2 :: String
action2 = "入店"

tweet :: [String] -> String -> String -> String -> String -> String
tweet c u j a a2 =
	textA ++ intercalate "、" c ++ textB ++ show (length c) ++
	u ++ textC ++ j ++ textD ++ j ++ textE ++ a ++ textF ++
	a2 ++ textG
