module Main where

import Lecture

subtitle :: String
subtitle = "第10回 ghcの使いかた"

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
	text t "* ghcとはHaskellの", \t -> do
	itext t 1 "- コンパイラ", \t -> do
	itext t 1 "- インタプリタ", \t -> do
	itext t 1 "- 対話環境", \t -> do
	itext t 1 "- プロファイラ", \t -> do
	itext t 1 "- これらすべてである", \t -> do
	text t "* 簡単なmake的な機能もある", \t -> do
	itext t 1 "- ファイルの依存性をチェックして最少限のビルド", \t -> do
	text t "* よってオプションも多岐に渡る", \t -> do
	text t "* ある程度しぼって、それらの機能を見ていこう"
 ]
