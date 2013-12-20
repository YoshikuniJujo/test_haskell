import Lecture

subtitle :: String
subtitle = "第45回 hsc2hs"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* hsc2hsという前処理用のプログラムがある", \t -> do
	text t "* C言語を扱う場合には", \t -> do
	itext t 1 "- マクロから逃れることはできない", \t -> do
	itext t 1 "- 構造体の構造はC言語内からしか見えない", \t -> do
	arrowIText t 1 "Haskell上でマジックナンバーを使うことになる", \t -> do
	text t "* それは避けたいので、前処理で", \t -> do
	itext t 1 "- マクロ展開や構造体に関する計算を行う"
 ]
