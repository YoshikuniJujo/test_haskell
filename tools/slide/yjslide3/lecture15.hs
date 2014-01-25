import Data.Char

import Lecture

subtitle :: String
subtitle = "第15回 高階関数の操作"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2, prelude3,
	valToFun1, valToFun2, valToFun3, valToFun4, valToFunSummary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 単純な関数を高階関数に変えたり", \t -> do
	text t "* 引数を追加したり、減らしたり", \t -> do
	text t "* いろいろな変換を関数に対してすることができる", \t -> do
	text t "* 型と定義とを見くらべながらやっていく", \t -> do
	text t "* 次回「モナド」を学ぶときにこの操作を使うとわかりやすい", \t -> do
	text t "* 関数に対するいろいろな変換を見ていこう"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "(->)は右結合"
	text t "", \t -> do
	text t "* 関数に対する変換を見ていくわけだが", \t -> do
	text t "* ひとつ頭に置いておくと良いことがある", \t -> do
	text t "* 型を作るときに使われる(->)は右結合である", \t -> do
	text t "* 以下のふたつの定義は同じこと", \t -> do
	itext t 1 "a -> b -> c", \t -> do
	itext t 1 "a -> (b -> c)", \t -> do
	text t "* 以下のふたつが同じということ", \t -> do
	itext t 1 "- 引数を2つとる関数", \t -> do
	itext t 1 "- 引数を1つとり「引数を1つとる関数」を返す関数"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "curryの2つの見方", \t -> do
	text t "* curryを例にして見てみよう", \t -> do
	itext t 1 "curry :: ((a, b) -> c) -> (a -> b -> c)", \t -> do
	itext t 1 "curry f = \\x y -> f (x, y)", \t -> do
	text t "* これは以下のように考えられる", \t -> do
	itext t 1 "- タプルをとる関数を", \t -> do
	itext t 1 "- 引数を2つとる関数に変換する関数", \t -> do
	text t "* 全く同じことを以下のようにも書ける", \t -> do
	itext t 1 "curry :: ((a, b) -> c) -> a -> b -> c", \t -> do
	itext t 1 "curry f x y = f (x, y)", \t -> do
	text t "* 同じことだが、この表記だと以下のように読める", \t -> do
	itext t 1 "- タプルをとる関数の他に引数を2つとり", \t -> do
	itext t 1 "- それらをタプルにし第一引数の関数を適用する関数"
 ]

valToFun1 :: Page
valToFun1 = [\t -> do
	writeTopTitle t "単純な値"
	text t "", \t -> do
	text t "* たとえば以下の関数について考えよう", \t -> do
	itext t 1 "eight :: (Int -> b) -> b", \t -> do
	itext t 1 "eight f = f 8", \t -> do
	text t "* lectures/lecture15ディレクトリを作成し", \t -> do
	text t "* transFuns.hsに書き込もう", \t -> do
	text t "* eightは引数の関数に8を適用する関数", \t -> do
	text t "* しかし、これは単なる8とほとんど同じものである"
 ]

eight :: (Int -> b) -> b
eight f = f 8

valToFun2 :: Page
valToFun2 = [\t -> do
	writeTopTitle t "単純な値"
	text t "", \t -> do
	text t "* 「単純な値とほとんど同じものである」ことの確認", \t -> do
	itext t 1 "% ghci transFuns.hs", \t -> do
	itext t 1 "*Main> eight (+ 5)", \t -> do
	itext t 1 $ show $ eight (+ 5), \t -> do
	itext t 1 "*Main> eight even", \t -> do
	itext t 1 $ show $ eight even, \t -> do
	itext t 1 "*Main> eight id", \t -> do
	itext t 1 $ show $ eight id
 ]

valToFun3 :: Page
valToFun3 = [\t -> do
	writeTopTitle t "単純な値"
	text t "", \t -> do
	text t "* すべての値がeightと同じような形に変換できる", \t -> do
	text t "* もとの値をxとし型をaとすると", \t -> do
	itext t 1 "x :: a", \t -> do
	itext t 1 "funX :: (a -> b) -> b", \t -> do
	itext t 1 "funX f = f x", \t -> do
	text t "* この変換を行う関数を作ってみよう", \t -> do
	itext t 1 "valToFun :: a -> ((a -> b) -> b)", \t -> do
	itext t 1 "valToFun x = \\f -> f x", \t -> do
	text t "* 同じことを以下のようにも書ける", \t -> do
	itext t 1 "valToFun :: a -> (a -> b) -> b", \t -> do
	itext t 1 "valToFun x f = f x", \t -> do
	text t "* どちらかの定義をtransFuns.hsに書き込もう"
 ]

valToFun :: a -> (a -> b) -> b
valToFun x f = f x

three :: (Int -> b) -> b
three = valToFun 3

c :: (Char -> b) -> b
c = valToFun 'c'

valToFun4 :: Page
valToFun4 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> let three =valToFun 3", \t -> do
	itext t 1 "*Main> three (+ 7)", \t -> do
	itext t 1 $ show $ three (+ 7), \t -> do
	itext t 1 "*Main> three even", \t -> do
	itext t 1 $ show $ three even, \t -> do
	itext t 1 "*Main> :load + Data.Char", \t -> do
	itext t 1 "*Main Data.Char> let c = valToFun 'c'", \t -> do
	itext t 1 "*Main Data.Char> c isLower", \t -> do
	itext t 1 $ show $ c isLower
 ]

valToFunSummary :: Page
valToFunSummary = [\t -> do
	writeTopTitle t "単純な値(まとめ)"
	text t "", \t -> do
	text t "* 以下の2つの型はほぼ同じものと考えられる", \t -> do
	itext t 1 "a", \t -> do
	itext t 1 "(a -> b) -> b", \t -> do
	text t "* 前者を後者の形式に変換する関数valToFunを定義した", \t -> do
	itext t 1 "valToFun :: a -> (a -> b) -> b", \t -> do
	itext t 1 "valToFun x f = f x", \t -> do
	text t "* もしも以下のような形の関数の定義を見たら"
	itext t 1 "単純な値に置き換えられるということ", \t -> do
	itext t 1 "(Int -> b) -> b", \t -> do
	itext t 1 "(Char -> b) -> b", \t -> do
	itext t 1 "([Int] -> b) -> b", \t -> do
	itext t 1 "((Int -> Char) -> b) -> b"
 ]
