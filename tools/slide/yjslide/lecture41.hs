import Lecture

subtitle :: String
subtitle = "第41回 unbox型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ghcにおいて普通の値はbox化された値である", \t -> do
	text t "* これは遅延評価のためにraw bitの表現を包み込んである", \t -> do
	text t "* そのraw bitの表現がunbox型である", \t -> do
	text t "* ここらへんの最適化は(-O)でghcがしてくれる", \t -> do
	text t "* ghcの最適化がうまく動かないときだけunbox型を使う"
 ]
