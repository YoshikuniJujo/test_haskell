import Lecture

subtitle :: String
subtitle = "第3回 タプル"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 複数の値をまとめた構造をひとつの値として扱える", \t -> do
	text t "* その構造をタプルと呼ぶ", \t -> do
	text t "* タプルを引数として取るときにはパターンマッチが使える", \t -> do
	text t "* タプルとして引数をまとめることができる", \t -> do
	text t "* その逆もでき、それをカリー化と呼ぶ"
 ]
