module Main where

import Lecture

subtitle :: String
subtitle = "第1回 手続き型から関数型へ"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* プログラミング言語の進化の歴史は", \t -> do
	itext t 1 "- 手続き型から関数型への移行の歴史とも言える"
 ]
