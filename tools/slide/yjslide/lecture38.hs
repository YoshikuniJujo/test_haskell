import Lecture

subtitle :: String
subtitle = "第38回 MultiParamTypeClasses拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 複数の型をまきこんだクラスを定義することができる", \t -> do
	text t "* そのためにはMultiParamTypeClasses拡張が必要", \t -> do
	text t "* 型同士の関係を表していると考えることができる"
 ]
