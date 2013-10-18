module Main where

import Lecture

subtitle :: String
subtitle = "第12回 テスト"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, typeCheck
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでは型システム自体が一種のテストである", \t -> do
	itext t 1 "- 適切な型の設計が重要", \t -> do
	text t "* 関数は単なる置き換えと考えられるので", \t -> do
	itext t 1 "- プログラムの「正しさ」をある程度「証明」できる", \t -> do
	itext t 1 "- 手作業での簡約が証明となる", \t -> do
	text t "* 数学的に扱えない部分についてはテストが必要となる", \t -> do
	itext t 1 "- 入力の型(つまり範囲)が決まっている", \t -> do
	arrowIText t 2 "ランダムな値を生成してテストすることが可能", \t -> do
	arrowIText t 2 "QuickCheck", \t -> do
	itext t 1 "- IOが絡むもの、境界条件などの決まった値のテスト", \t -> do
	arrowIText t 2 "HUnit"
 ]

typeCheck :: Page
typeCheck = [\t -> do
	writeTopTitle t "型によるチェック", \t -> do
	text t "* 型の不整合によるバグ", \t -> do
	itext t 1 "- 動的型付けの言語では起こり得る", \t -> do
	itext t 1 "- 静的型付けの言語ではコンパイル時にチェック", \t -> do
	text t "例: (python)"
	text t "x = input(\"string/number? \")"
	text t "if x == \"string\":"
	itext t 1 "y = \"Hello\""
	text t "else:"
	itext t 1 "y = 88"
	text t "print(y + \", world!\")"
 ]
