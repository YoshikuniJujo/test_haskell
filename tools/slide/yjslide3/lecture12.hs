import Lecture

subtitle :: String
subtitle = "第12回 引数をとる型構築子"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回は代数的データ型について見た", \t -> do
	text t "* 以下のような構文を学んだ", \t -> do
	itext t 1 "data [型構築子]"
	itext t 2 "= [値構築子1] [型11] [型12] ..."
	itext t 2 "| [値構築子2] [型21] [型22] ..."
	itext t 2 "...", \t -> do
	text t "* 今回はこれを拡張し", \t -> do
	itext t 1 "- より柔軟性のある定義をする", \t -> do
	itext t 1 "- 「構造」を抽象化した型を作る"
 ]
