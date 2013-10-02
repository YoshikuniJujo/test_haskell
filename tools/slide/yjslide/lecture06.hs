module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第6回 入出力"

pages :: [Page]
pages = [
	titlePage, term, prelude1, prelude2, prelude2_5, prelude3
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

term :: Page
term = [\t -> do
	writeTopTitle t "用語について"
	text t "関数の入出力とプログラムの入出力がまぎらわしいので"
	text t "", \t -> do
	itext t 1 "関数の入力、出力、入出力", \t -> do
	dvArrowShort t
	itext t 1 "そのまま入力、出力、入出力"
	text t "", \t -> do
	itext t 1 "プログラムの入力、出力、入出力", \t -> do
	dvArrowShort t
	itext t 1 "インプット、アウトプット、I/O"
	text t ""
	text t "とする"
 ]

prelude1 :: Page
prelude1 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	semititle t "* HaskellではIO monadという仕組みを使う", \t -> do
	semititle t "* 理解しづらいことで有名", \t -> do
	itext t 1 "まぎらわしさがある"
	itext t 1 "説明のしかたがまずい"
	dvArrowShort t
	text t "「わかりやすく説明してみようじゃないか」という野望"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t ""
	text t "まずはmonadを説明し、次にIO monadを説明する", \t -> do
	xmark t "まずはmonadを説明し、次にIO monadを説明する"
	arrowIText t 1 "monadは「実のところ」IO monadの本質ではない"
	text t "", \t -> do
	text t "むしろ"
	text t ""
	semititle t "「I/Oの仕組みを作ったところ、"
	semititle t "それがmonadという大きな枠組にはまりこんだ」"
	itext t 5 ""
	itext t 5 "が正解"
 ]

prelude2_5 :: Page
prelude2_5 = [\t -> do
	writeTopTitle t "はじめに"
	semititle t "説明の戦略", \t -> do
	semititle t "* I/Oを実現させた漢たちの架空の歴史をたどる", \t -> do
	semititle t "* 実際の歴史がどうだったかは知らない", \t -> do
	semititle t "* 今の形に至るまでの思考経路を想像", \t -> do
	dvArrowShort t
	semititle t "研究者達の(架空の)思考を追体験する", \t -> do
	dvArrowShort t
	semititle t "「なるほど!」と思えるかもしれない"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "はじめに"
	text t ""
	semititle t "「純粋関数型言語でI/Oはどうする?」", \t -> do
	text t "- 他の言語では副作用という形でI/Oを実現している", \t -> do
	text t "- しかし、副作用を許すと参照透過性が確保できない", \t -> do
	text t "- 遅延性があるのでI/Oがいつ実行されるか予想しづらい", \t -> do
	dvArrowShort t
	semititle t "特別な仕組みが必要"
	itext t 1 "- 無限リスト"
	itext t 1 "- I/Oを行う機械または装置というメタファ"
 ]
