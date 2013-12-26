module Main where

import Lecture

subtitle :: String
subtitle = "第1回 手続き型から関数型へ"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	[flip writeTitle subtitle], prelude, prelude2
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* プログラミング言語の進化の歴史は", \t -> do
	itext t 1 "- 手続き型から関数型への移行の歴史とも言える", \t -> do
	text t "* 手続き型は「何をするか」を指定する", \t -> do
	text t "* 関数型は「何であるか」を指定する", \t -> do
	text t "* 小規模なプログラムであれば手続き型のほうが簡単", \t -> do
	text t "* 規模が大きくなると手続き型では対応できなくなる", \t -> do
	itext t 1 "- 関数型プログラミングが必要となる"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 手続き型言語ではひとつひとつの手順を指定する", \t -> do
	text t "* 関数型言語では「何を作りたいか」という目標を指定する", \t -> do
	text t "* アセンブリ言語は純粋手続き型言語"
 ]
