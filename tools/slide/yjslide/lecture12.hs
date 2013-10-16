module Main where

import Lecture

subtitle :: String
subtitle = "第12回 テスト"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude
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
	arrowIText t 1 "ランダムな値を生成してテストすることが可能", \t -> do
	arrowIText t 1 "QuickCheck", \t -> do
	itext t 1 ""
 ]
