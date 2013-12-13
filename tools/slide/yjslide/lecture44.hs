import Lecture

subtitle :: String
subtitle = "第44回 C言語とのインターフェース3"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今回はwrapperとStablePtrについて説明する", \t -> do
	text t "* wrapperはHaskellの関数を関数ポインタにする方法", \t -> do
	itext t 1 "- できた関数ポインタはCの関数にわたすことができる", \t -> do
	text t "* StablePtrはHaskellの値をそのままの形でポインタにする", \t -> do
	itext t 1 "- なかの値はHaskell内でしか扱えない", \t -> do
	itext t 1 "- C言語内でHaskell関数を呼び出し、返ってきた値を", \t -> do
	itext t 1 "- そのままの形でHaskell関数にわたす"
 ]
