import Lecture

subtitle :: String
subtitle = "高階関数の操作"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 単純な関数を高階関数に変えたり", \t -> do
	text t "* 引数を追加したり、減らしたり", \t -> do
	text t "* いろいろな変換を関数に対してすることができる", \t -> do
	text t "* 型と定義とを見くらべながらやっていく", \t -> do
	text t "* 次回「モナド」を学ぶときにこの操作を使うとわかりやすい", \t -> do
	text t "* 高階関数に対するいろいろな変換を見ていこう"
 ]
