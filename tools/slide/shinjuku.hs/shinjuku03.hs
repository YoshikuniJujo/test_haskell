import Lecture

subtitle :: String
subtitle = "3. 多相関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, sipmlest1, simplest2,
	const1, flip1, partil1, partial2, partial3,
	apply1, apply2, dot1, dot2, summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 関数はその引数や返り値の型が決まっている", \t -> do
	text t "* 引数や返り値に複数の型の値をとれる関数がある", \t -> do
	text t "* それを多相関数と呼ぶ", \t -> do
	text t "* 多相性には2種類ある", \t -> do
	itext t 1 "+ パラメトリック多相", \t -> do
	itext t 1 "+ アドホック多相", \t -> do
	text t "* パラメトリック多相について説明する", \t -> do
	text t "* アドホック多相については型クラスのところで説明する"
	]

sipmlest1 :: Page
sipmlest1 = [ \t -> do
	writeTopTitle t "Let it be"
	text t "", \t -> do
	text t "* 関数とは値を値に対応づけるもの", \t -> do
	text t "* 値をそれ自身に対応づける関数というものも考えられる", \t -> do
	itext t 1 "id x = x", \t -> do
	text t "* この関数は定義ずみの関数である", \t -> do
	itext t 1 "*Main> id 8", \t -> do
	itext t 1 "8", \t -> do
	itext t 1 "*Main> id True", \t -> do
	itext t 1 "True", \t -> do
	itext t 1 "*Main> id 'c'", \t -> do
	itext t 1 "'c'"
	]

simplest2 :: Page
simplest2 = [ \t -> do
	writeTopTitle t "型変数"
	text t "", \t -> do
	text t "* 関数idの型はどうなっているだろうか", \t -> do
	text t "* id TrueであればBool値をとってBool値をかえしているので", \t -> do
	itext t 1 "Bool -> Bool", \t -> do
	text t "* id 'c'であればChar値をとってChar値をかえしているので", \t -> do
	itext t 1 "Char -> Char", \t -> do
	text t "* 型は何でもいいが引数と返り値の型は一致する必要がある", \t -> do
	text t "* 関数idの型は以下のように書くことができる", \t -> do
	itext t 1 "id :: a -> a", \t -> do
	text t "* このaは型変数である", \t -> do
	text t "* 型が大文字ではじまるのに対して型変数は小文字ではじまる", \t -> do
	text t "* 同じ型変数は同じ型であることを示す"
	]

const1 :: Page
const1 = [ \t -> do
	writeTopTitle t "値を無視する関数"
	text t "", \t -> do
	text t "* 第2引数を無視する関数がある", \t -> do
	itext t 1 "const x y = x", \t -> do
	text t "* この関数も定義ずみだ", \t -> do
	itext t 1 "*Main> const 8 True", \t -> do
	itext t 1 "8", \t -> do
	itext t 1 "*Main> const False 'c'", \t -> do
	itext t 1 "False", \t -> do
	text t "* この関数の型は以下のようになる", \t -> do
	itext t 1 "const :: a -> b -> a", \t -> do
	text t "* 第1引数の型が返り値の型と一致する必要がある", \t -> do
	text t "* 第2引数は無視されるので型は何でもいい"
	]

flip1 :: Page
flip1 = [ \t -> do
	writeTopTitle t "引数をいれかえる関数"
	text t "", \t -> do
	text t "* 2引数関数の引数の順番をいれかえる関数がある", \t -> do
	itext t 1 "flip f x y = f y x", \t -> do
	text t "* この関数も定義ずみだ", \t -> do
	itext t 1 "*Main> flip (-) 3 5", \t -> do
	itext t 1 "2", \t -> do
	itext t 1 "*Main> flip (/) 2 15", \t -> do
	itext t 1 "7.5", \t -> do
	text t "* この関数の型は以下のようになる", \t -> do
	itext t 1 "flip :: (b -> a -> c) -> a -> b -> c", \t -> do
	text t "* 関数flipの定義と見くらべてみよう", \t -> do
	text t "* 引数f, x, yはそれぞれどの型変数に対応するだろうか"
	]

partil1 :: Page
partil1 = [ \t -> do
	writeTopTitle t "演算子の部分適用"
	text t "", \t -> do
	text t "* ここで「演算子の部分適用」という構文糖を見てみよう", \t -> do
	text t "* 「関数の部分適用」という言葉がある", \t -> do
	text t "* 2引数関数とはそもそも「関数をかえす関数」なので", \t -> do
	itext t 1 "引数をひとつだけあたえることで1引数関数となる", \t -> do
	text t "* これは2つの引数のうち1つだけあたえたように見えるので", \t -> do
	itext t 1 "「関数の部分適用」と呼ばれる", \t -> do
	text t "* 演算子は()で関数となるので部分適用ができる", \t -> do
	itext t 1 "*Main> let add3 = (+) 3", \t -> do
	itext t 1 "*Main> add3 7", \t -> do
	itext t 1 "10", \t -> do
	text t "* 対話環境ではletを使って変数定義が可能だ"
	]

partial2 :: Page
partial2 = [ \t -> do
	writeTopTitle t "演算子の部分適用"
	text t "", \t -> do
	text t "* 演算子については部分適用のための構文糖がある", \t -> do
	itext t 1 "(+) 3", \t -> do
	arrowIText t 1 "(3 +)", \t -> do
	text t "* 演算子の後ろに何も置かずに括弧を閉じることで", \t -> do
	itext t 1 "「ここの値が残ってるよ」という感じを出している", \t -> do
	text t "* 演算子(/)も同じだ", \t -> do
	itext t 1 "(/) 3", \t -> do
	arrowIText t 1 "(3 /)", \t -> do
	text t "* これは3を割る関数だ"
	]

partial3 :: Page
partial3 = [ \t -> do
	writeTopTitle t "演算子の部分適用"
	text t "", \t -> do
	text t "* 演算子の左側だけ値をあたえた", \t -> do
	text t "* 同様に右側にだけ値をあたえることもできる", \t -> do
	itext t 1 "(/ 3)は3で割る関数となる", \t -> do
	text t "* これは脱糖すると以下のようになる", \t -> do
	itext t 1 "(/ 3)", \t -> do
	arrowIText t 1 "flip (/) 3", \t -> do
	text t "* 演算子(関数)の右側(第2引数)について部分適用したいとき", \t -> do
	itext t 1 "この記法でシンプルに書ける"
	]

apply1 :: Page
apply1 = [ \t -> do
	writeTopTitle t "関数適用演算子"
	text t "", \t -> do
	text t "* 関数適用を行う演算子($)がある", \t -> do
	itext t 1 "f $ x = f x", \t -> do
	text t "* 対話環境で試してみよう", \t -> do
	itext t 1 "*Main> not $ False", \t -> do
	itext t 1 "True", \t -> do
	itext t 1 "*Main> negate $ 8", \t -> do
	itext t 1 "-8", \t -> do
	text t "* 関数的に定義すると以下のようになる", \t -> do
	itext t 1 "($) f x = f x", \t -> do
	text t "* 第1引数の関数fに第2引数の値xをあたえている"
	]

apply2 :: Page
apply2 = [ \t -> do
	writeTopTitle t "関数適用演算子"
	text t "", \t -> do
	text t "* 演算子($)に何の意味があるのか?", \t -> do
	itext t 1 "f (g x)", \t -> do
	arrowIText t 1 "f $ g x", \t -> do
	text t "* 括弧を消すことができる", \t -> do
	text t "* 演算子($)は結合力が弱く右結合である", \t -> do
	itext t 1 "f (g (x + y * z))"
	arrowIText t 1 "f $ g $ x + y * z", \t -> do
	text t "* $の右側が評価された値が$の左側の関数の引数になる", \t -> do
	text t "* 定義は以下のようになっていた", \t -> do
	itext t 1 "($) f x = f x", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "($) :: (a -> b) -> a -> b"
	]

dot1 :: Page
dot1 = [ \t -> do
	writeTopTitle t "関数合成演算子"
	text t "", \t -> do
	text t "* 関数を合成する演算子(.)がある", \t -> do
	itext t 1 "(f . g) x = f (g x)", \t -> do
	text t "* 関数gの返り値が関数fにわたされる", \t -> do
	text t "* たとえば2倍して3足す関数は以下のように書ける", \t -> do
	itext t 1 "(+ 3) . (* 2)", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "(.) :: (b -> c) -> (a -> b) -> a -> c"
	]

dot2 :: Page
dot2 = [ \t -> do
	writeTopTitle t "関数合成演算子"
	text t "", \t -> do
	text t "* 演算子($)の連続は演算子(.)におきかえられる", \t -> do
	itext t 1 "(/ 5) $ (+ 3) $ (* 2) 4", \t -> do
	arrowIText t 1 "(/ 5) . (+ 3) $ (* 2) 4", \t -> do
	text t "* 以下が同じということだ", \t -> do
	itext t 1 "+ 値8に3を足した結果を5で割る", \t -> do
	itext t 1 "+ 「3足す」と「5で割る」を合成しそれに8をあたえる", \t -> do
	text t "* 意味的にも見た目的にも後者のほうがきれいなので", \t -> do
	itext t 1 "後者を採用する"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 複数の型に対して適用可能な多相関数がある", \t -> do
	text t "* 多相関数の型宣言には型変数を使う", \t -> do
	text t "* いくつかの多相関数を見た", \t -> do
	text t "* これらの多相関数を使うことでコードが簡潔になる"
	]
