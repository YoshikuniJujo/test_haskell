import Lecture

subtitle :: String
subtitle = "トライアル 第3.6回 いろいろな関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, funId
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 関数を理解することがHaskellを理解する鍵である", \t -> do
	text t "* Haskellでは高階関数を多用する", \t -> do
	text t "* 高階関数について理解するには", \t -> do
	itext t 1 "何よりも「慣れ」が必要である", \t -> do
	text t "* 今までの思考の習慣を打破しなければならない", \t -> do
	text t "* 簡単なものから関数について見ていこう"
 ]

-- id, ($), const, flip, curry, uncurry, (.)

funId :: Page
funId = [\t -> do
	writeTopTitle t "id"
	text t "", \t -> do
	text t "* まずは関数idについて見てみよう", \t -> do
	text t "* この関数は引数をそのまま返す関数", \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "id :: a -> a", \t -> do
	itext t 1 "id x = x"
 ]
