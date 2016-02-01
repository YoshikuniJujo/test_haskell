import Lecture

subtitle :: String
subtitle = "12. IOモナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, output, addInput
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* IOモナドの真実を学ぶ", \t -> do
	text t "* たとえば以下のような関数", \t -> do
	itext t 1 "putStr :: String -> IO ()", \t -> do
	itext t 0.5 "が何を意味しているのか", \t -> do
	text t "* 多くのHaskellerが誤解している", \t -> do
	text t "* 関数putStrは文字列を表示する関数なのだろうか", \t -> do
	text t "* putStr \"hello\"が評価されることで", \t -> do
	itext t 1 "端末に\"hello\"と表示されるという理解でいいのか", \t -> do
	text t "* もつれた糸をときほぐしていこう"
	]

output :: Page
output = [ \t -> do
	writeTopTitle t "順番に出力"
	text t "", \t -> do
	text t "* たとえばMachine型という型があったとする", \t -> do
	text t "* Machine型の値としてputHelloとputWorldがある", \t -> do
	itext t 1 "putHello :: Machine", \t -> do
	itext t 1 "putWorld :: Machine", \t -> do
	text t "* それぞれ\"Hello\"と\"World\"を出力する機械だ", \t -> do
	text t "* これらの値をつなぐ関数nextがあると", \t -> do
	itext t 1 "next :: Machine -> Machine -> Machine", \t -> do
	text t "* \"HelloWorld\"と表示する関数は以下のようになる", \t -> do
	itext t 1 "putHelloWorld :: Machine", \t -> do
	itext t 1 "putHelloWorld = putHello `next` putWorld"
	]

addInput :: Page
addInput = [ \t -> do
	writeTopTitle t "入力値を渡す"
	text t "", \t -> do
	text t "* 出力については関数nextでいい", \t -> do
	text t "* 入力についてはどうだろうか", \t -> do
	text t "* 入力を扱う機械を作り", \t -> do
	itext t 1 "得られた値を次の機械に渡せばいい", \t -> do
	text t "* ひとつめの機械からふたつめの機械に値を渡す関数", \t -> do
	itext t 1 "(>>>) :: Machine -> Machine -> Machine", \t -> do
	itext t 0.5 "を考えよう"
	]

getPutLine :: Page
getPutLine = [ \t -> do
	writeTopTitle t ""
	]
