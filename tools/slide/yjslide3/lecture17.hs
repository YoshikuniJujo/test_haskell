import Lecture

subtitle :: String
subtitle = "第17回 IOモナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2, machine
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellは参照透過性を持つ言語である", \t -> do
	text t "* つまり、関数の返す値は引数が同じなら常に同じ", \t -> do
	text t "* また遅延評価する言語である", \t -> do
	text t "* つまり、関数が評価されるタイミングが予測しづらい", \t -> do
	text t "* 多くの言語では以下のような形で入出力を扱う", \t -> do
	itext t 1 "- 関数が「評価」されるタイミングで入出力を行い", \t -> do
	itext t 1 "- 関数の返り値として入力を返す", \t -> do
	text t "* 関数の返り値として入力を返すと参照透過性が破壊される", \t -> do
	text t "* 「評価」のタイミングで入出力を行うと", \t -> do
	itext t 1 "- 「評価」の順がプログラムの意味に影響を与える"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* どうすればいいのだろう?", \t -> do
	itext t 1 "- 入出力を行う機械という値を作れば良い", \t -> do
	itext t 1 "- 数値型は対話環境で評価するとその値を表示する", \t -> do
	itext t 1 "- 文字型も対話環境で評価するとその値を表示する", \t -> do
	itext t 1 "- 機械型は対話環境で評価するとその動作を行う", \t -> do
	text t "* 機械型を数値型と同じように基本的な型として持てば良い"
 ]

machine :: Page
machine = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* たとえばMachine型という型があったとする", \t -> do
	text t "* Machine型の値としてputHelloとputWorldがあったとする"
 ]
