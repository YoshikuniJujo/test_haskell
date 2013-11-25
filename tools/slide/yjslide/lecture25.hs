import Lecture

subtitle :: String
subtitle = "第25回 入出力例外"

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
	text t "* 前回までで例外処理の仕組みはだいたい学べた", \t -> do
	text t "* 今回は入出力例外にしぼって見ていく", \t -> do
	text t "* 歴史的な理由で以下のようになっている", \t -> do
	itext t 1 "type IOError = IOException", \t -> do
	text t "* IOErrorの種類や作りかた、情報の引き出しかた等を学ぶ"
 ]
