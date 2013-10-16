module Main where

import Lecture

subtitle :: String
subtitle = "第11回 ドキュメント"

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
	text t "* ドキュメントを書こう", \t -> do
	itext t 1 "- めんどくさい", \t -> do
	itext t 1 "- 日本語で書くと他言語の人にはノイズになる", \t -> do
	itext t 1 "- 英語は苦手", \t -> do
	itext t 1 "- 使用例くらいなら書けるかな", \t -> do
	text t "* haddockを使おう", \t -> do
	text t "* 関数の型は自動的に表示してくれる", \t -> do
	itext t 1 "- 仕事は半分終わったようなもの", \t -> do
	itext t 1 "- 使用例以外は暇なときに辞書でも引きながら書くか", \t -> do
	dvArrowShort t
	text t "僕自身、そんなのりです"
 ]


