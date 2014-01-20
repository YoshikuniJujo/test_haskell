import Lecture

subtitle :: String
subtitle = "第10回 演習(2日目)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今日は再帰関数について学んだ", \t -> do
	text t "* Haskellにおいてリストは重要なデータ構造である", \t -> do
	text t "* リストを扱う多くの関数が用意されている", \t -> do
	text t "* それらの関数も再帰的に定義されている", \t -> do
	text t "* リストを扱う関数の使いかたや作られかたを見た", \t -> do
	text t "* 演習ではいろいろな再帰関数を定義していく", \t -> do
	text t "* とくにリストを扱う関数をいろいろなやりかたで定義する"
 ]
