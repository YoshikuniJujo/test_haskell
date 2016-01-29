import Lecture

subtitle :: String
subtitle = "10. 関数の変形"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, add1, add2, rev, convert
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* モナドを理解するためにわかりやすい形からの変形を見る", \t -> do
	text t "* その変形のしかたをここで学ぶ", \t -> do
	text t "* 関数の本質を変えずに引数と返り値に対して", \t -> do
	itext t 1 "引数を追加したり削除したりできることを示す"
	]

add1 :: Page
add1 = [ \t -> do
	writeTopTitle t "足し算"
	text t "", \t -> do
	text t "* 足し算は2引数関数だ", \t -> do
	itext t 1 "(+) :: Integer -> Integer -> Integer", \t -> do
	text t "* 単純な操作でこれを以下の型に変えられる", \t -> do
	itext t 1 "add :: (a -> Integer) -> Integer -> a -> Integer", \t -> do
	text t "* 「『不定の型aの値をとって整数をかえす関数』と整数、", \t -> do
	itext t 1 "さらに型aの値をとって、整数をかえす」関数だ", \t -> do
	text t "* たとえば以下のようにすることで", \t -> do
	itext t 1 "length `add` 3", \t -> do
	itext t 0.5 "リストの長さに3を足す関数が作れる"
	]

add2 :: Page
add2 = [ \t -> do
	writeTopTitle t "足し算"
	text t "", \t -> do
	text t "* 関数addは関数(+)によって以下のように定義できる", \t -> do
	itext t 1 "add f n x = (+) (f x) n", \t -> do
	text t "* (+)のところを任意の2引数関数にかえることができる", \t -> do
	text t "* 以下のような型の関数を", \t -> do
	itext t 1 "X -> Y -> Z", \t -> do
	text t "* 以下のような型の関数に機械的に変換できる", \t -> do
	itext t 1 "(a -> X) -> Y -> a -> Z", \t -> do
	text t "* 引数と返り値の両方に引数をひとつ追加したといえる"
	]

rev :: Page
rev = [ \t -> do
	writeTopTitle t "引数を削除する"
	text t "", \t -> do
	text t "* 逆に以下のような関数を", \t -> do
	itext t 1 "funA :: (a -> X) -> Y -> a -> Z", \t -> do
	text t "* 以下のような型の関数に変換できる", \t -> do
	itext t 1 "fun :: X -> Y -> Z", \t -> do
	text t "* 変換してみよう", \t -> do
	itext t 1 "fun a b = funA (const a) b ()", \t -> do
	text t "* 何に対してもつねにaを返す関数を関数funAの引数とした", \t -> do
	text t "* 関数funAの第3引数はなんでもよい", \t -> do
	text t "* ダミーの引数としてユニット値とした"
	]

convert :: Page
convert = [ \t -> do
	writeTopTitle t "相互に変換"
	text t "", \t -> do
	text t "* 以下の2つの関数があるとき", \t -> do
	itext t 1 "fun :: X -> Y -> Z", \t -> do
	itext t 1 "funA :: (a -> X) -> Y -> a -> Z", \t -> do
	text t "* 相互に変換することができる", \t -> do
	itext t 1 "funA f b x = fun (f x) b", \t -> do
	itext t 1 "fun a b = funA (const a) b ()", \t -> do
	text t "* つまり関数funと関数funAとは同じ関数といえる"
	]
