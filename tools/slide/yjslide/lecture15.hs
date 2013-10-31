module Main where

import Lecture

subtitle :: String
subtitle = "第15回 いろいろなモナド"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, monadList0,
	monadList1 1,
	monadList1 2,
	monadList1 3,
	monadList1 4,
	monadList1 5,
	monadList1 6,
	monadList1 7,
	monadList1 8,
	monadList1 9
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* モナドは抽象的な概念", \t -> do
	text t "* それ自体としては理解しにくい", \t -> do
	text t "* これもモナドあれもモナドといろいろなモナドを見る", \t -> do
	text t "* その結果共通する構造が見えてくる", \t -> do
	text t "* 今回はいろいろなモナドについて見ていくことにする"
 ]

monadList :: [String]
monadList = [
	"Identity", "Maybe", "Error", "List", "State", "Reader", "Writer",
	"Cont", "IO"]

showMonadList :: Int -> Int -> String -> Turtle -> IO ()
showMonadList n i f = \t ->
	(if n == i then withRed t else id) $ text t $ show i ++ ". " ++ f

monadList0 :: Page
monadList0 = [\t -> writeTopTitle t "今回扱うモナド" >> text t ""] ++
	(zipWith (showMonadList 0) [1 ..] monadList)

monadList1 :: Int -> Page
monadList1 n = [\t -> oneshot t $ do
	writeTopTitle t "今回扱うモナド"
	oneshot t $ do
		text t ""
		mapM_ ($ t) $ (zipWith (showMonadList n) [1 ..] monadList)]
