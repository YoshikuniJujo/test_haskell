module Main where

import Lecture

subtitle :: String
subtitle = "第14回 モナド"

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
	text t "* IO monadの説明ではmonad自体の説明は省略", \t -> do
	itext t 1 "- IO monadの本質はmonadではない", \t -> do
	itext t 1 "- monadという方向からIOを見ると誤解が生じる", \t -> do
	itext t 1 "- IOは特殊なので他のmonadとは区別する必要がある"
	text t "", \t -> do
	text t "* monadとは何か?", \t -> do
	itext t 1 "以下の型の関数を持つ型m", \t -> do
	itext t 1 "return :: a -> m a"
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	text t "今は理解できなくていい", \t -> do
	arrowIText t 1 "モナドの理解はじわじわと深めていけば良い"
 ]
