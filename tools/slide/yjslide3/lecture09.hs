import Data.List

import Lecture

subtitle :: String
subtitle = "第9回 リストの再帰的定義"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	fib1, fib2, syntax, function, fib3, fib4,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回はリストを作成する関数を見た", \t -> do
	text t "* 今回は引数無しで直接無限リストを作成する方法を見る"
 ]

fib1 :: Page
fib1 = [\t -> do
	writeTopTitle t "フィボナッチ数列"
	text t "", \t -> do
	text t "* フィボナッチ数列とは", \t -> do
	itext t 1 "- 次の数が前の2項の和となる数列", \t -> do
	itext t 1 "0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89 ...", \t -> do
	text t "* 以下のように計算される", \t -> do
	itext t 1 "- 0, 1のつぎは0 + 1で1", \t -> do
	itext t 1 "- 1, 1のつぎは1 + 1で2", \t -> do
	itext t 1 "- 1, 2のつぎは1 + 2で3", \t -> do
	itext t 1 "- 2, 3のつぎは2 + 3で5", \t -> do
	itext t 1 "- 3, 5のつぎは3 + 5で8", \t -> do
	itext t 1 "- 5, 8のつぎは5 + 8で13", \t -> do
	itext t 1 "- 8, 13のつぎは8 + 13で21"
 ]

fib2 :: Page
fib2 = [\t -> do
	writeTopTitle t "戦略"
	text t "", \t -> do
	text t "* フィボナッチ数列の最初の要素を落としたtfibsを考える", \t -> do
	text t "* 以下の式が成り立つ", \t -> do
	itext t 2 "fibs  = 0, 1, 1, 2, 3,  5,  8, 13 ...", \t -> do
	itext t 1 "+"
	preLine t
	itext t 2 "tfibs = 1, 1, 2, 3, 5,  8, 13, 21 ...", \t -> do
	hLine t 20 70
	itext t 1.2 "fibs = 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ...", \t -> do
	text t "* つまりfibsは0, 1のあとに", \t -> do
	itext t 1 "fibsとtfibsのそれぞれの要素を足したもの"
 ]

syntax :: Page
syntax = [\t -> do
	writeTopTitle t "新たに必要になる構文"
	text t "", \t -> do
	text t "* 以下のような例を考える", \t -> do
	itext t 1 "dupHead (x : xs) = x : (x : xs)", \t -> do
	text t "* xとxsに分割したうえで再度(x : xs)で結合している", \t -> do
	text t "* ここの重複をなくしたい、つまり", \t -> do
	text t "* 変数への束縛と同時にパターンマッチしたい", \t -> do
	text t "* '@'を使えば実現できる", \t -> do
	itext t 1 "dupHead xa@(x : xs) = x : xa"
 ]

function :: Page
function = [\t -> do
	writeTopTitle t "新たに必要になる関数"
	text t "", \t -> do
	text t "* 2つのリストの各要素を"
	itext t 1 "演算子で結合した値を要素とするリストが欲しい", \t -> do
	text t "* そのための関数が用意されている", \t -> do
	itext t 1 "zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]", \t -> do
	itext t 1 "*Main> zipWith (*) [1, 2, 3] [4, 5, 6]", \t -> do
	itext t 1 $ show $ zipWith (*) [1 :: Int, 2, 3] [4, 5, 6]
 ]

fib3 :: Page
fib3 = [\t -> do
	writeTopTitle t "フィボナッチ数列"
	text t "", \t -> do
	text t "* fibsとtfibsがすでにあるとしてfibsは以下のようになる", \t -> do
	itext t 1 "fibs = 0 : 1 : zipWith (+) fibs tfibs", \t -> do
	text t "* fibsはこれで定義されるがtfibsがまだ定義できていない", \t -> do
	text t "* tfibsはfibsのtail部分なので", \t -> do
	itext t 1 "fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs"
 ]

fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs

fib4 :: Page
fib4 = [\t -> do
	writeTopTitle t "フィボナッチ数列"
	text t "", \t -> do
	text t "* lectures/lecture09を作成しそこに移動", \t -> do
	text t "* fib.hsに以下を書き込み", \t -> do
	itext t 1 "fibs, tfibs :: [Integer]"
	itext t 1 "fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs", \t -> do
	text t "* 別ウィンドウで", \t -> do
	itext t 1 "% ghci fib.hs", \t -> do
	itext t 1 "*Main> take 30 fibs", \t -> do
	mapM_ (itext t 1) $ flip unfoldr (show $ take 30 fibs) $ \str ->
		case str of
			[] -> Nothing
			_ -> Just $ splitAt 45 str
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* リストそのものを再帰的に定義することができる", \t -> do
	text t "* フィボナッチ数列の直接的な定義を見た"
 ]
