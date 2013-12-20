import Lecture

subtitle :: String
subtitle = "第46回 パッケージ化"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ライブラリやアプリケーションを作ったとき", \t -> do
	text t "* それを簡単にパッケージ化できる仕組みが欲しい", \t -> do
	text t "* Haskellにはcabalがある"
 ]
