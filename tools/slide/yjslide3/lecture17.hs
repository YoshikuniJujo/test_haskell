import Lecture

subtitle :: String
subtitle = "第17回 モナドの復習と演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回はモナドについて学んだ", \t -> do
	text t "* すこし難しかったかもしれない", \t -> do
	text t "* 今回はもうすこし簡単な例を見る", \t -> do
	text t "* また演習問題を解くことで理解することを試みよう"
 ]
