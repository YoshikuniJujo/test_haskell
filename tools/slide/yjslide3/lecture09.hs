import Lecture

subtitle :: String
subtitle = "第9回 リストの再帰的定義"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回はリストを作成する関数を見た", \t -> do
	text t "* 今回は引数無しで直接無限リストを作成する方法を見る"
 ]
