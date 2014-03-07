{-# LANGUAGE RankNTypes #-}

import Data.Char

import Lecture

subtitle :: String
subtitle = "第15回 関数の変換"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2, prelude3,
	unit, unit2, unit3, unitSummary,
	valToFun1, valToFun2, valToFun3, valToFun4, valToFun5,
	valToFunSummary,
	aboutAddArg1, aboutAddArg2, aboutAddArg3, aboutAddArg4, aboutAddArg5,
	aboutAddArg6, aboutAddArgSummary,
	aboutAddArg21, aboutAddArg22, aboutAddArg23, aboutAddArg2Summary,
	summary
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

unit :: Page
unit = [\t -> do
	writeTopTitle t "()"
	text t "", \t -> do
	text t "* ()という値をひとつだけ持つ()という型がある", \t -> do
	text t "* ()以外の値は持たない", \t -> do
	text t "* Bool型はFalseとTrueの2つの値を取るので", \t -> do
	itext t 1 "- ()型はBool型以上に単純な型と言える", \t -> do
	text t "* この値はあってもなくても変わらない", \t -> do
	text t "* よって以下はどれも同じものと考えられる", \t -> do
	itext t 1 "a", \t -> do
	itext t 1 "() -> a", \t -> do
	itext t 1 "(a, ())"
 ]

unit2 :: Page
unit2 = [\t -> do
	writeTopTitle t "()"
	text t "", \t -> do
	text t "* 以下の2つが同じであることを確認してみる", \t -> do
	itext t 1 "val :: a", \t -> do
	itext t 1 "fun :: () -> a", \t -> do
	text t "* 追加の情報なしに互いに定義できれば", \t -> do
	itext t 1 "- それらは「同じ」であると言うことができる", \t -> do
	text t "* funをvalを使って定義してみる", \t -> do
	itext t 1 "fun () = val", \t -> do
	text t "* valをfunを使って定義してみる", \t -> do
	itext t 1 "val = fun ()", \t -> do
	text t "* 互いに定義できたのでfunとvalは同じものと考えられる"
 ]

unit3 :: Page
unit3 = [\t -> do
	writeTopTitle t "()"
	text t "", \t -> do
	text t "* 以下の2つについても確認しておこう", \t -> do
	itext t 1 "val :: a", \t -> do
	itext t 1 "tup :: (a, ())", \t -> do
	text t "* tupをvalを使って定義してみる", \t -> do
	itext t 1 "tup = (val, ())", \t -> do
	text t "* valをtupを使って定義してみる", \t -> do
	itext t 1 "val = fst tup", \t -> do
	text t "* よってaと(a, ())は同じである"
 ]

unitSummary :: Page
unitSummary = [\t -> do
	writeTopTitle t "()(まとめ)"
	text t "", \t -> do
	text t "* Bool型はTrueかFalseのどちらかなので", \t -> do
	itext t 1 "- どちらかがわかれば情報が増えることになる", \t -> do
	text t "* ()型には()しかないので", \t -> do
	itext t 1 "- 「()だ!」と言っても情報は増えない", \t -> do
	text t "* よって()型は", \t -> do
	itext t 1 "- 引数の位置に出てきても", \t -> do
	itext t 1 "- タプルのなかに出てきても取り除くことができる", \t -> do
	text t "* 型同士が「同じ」ものであることを", \t -> do
	itext t 1 "- 追加の情報なしに", \t -> do
	itext t 1 "- 互いに相手を定義することで確かめられる"
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
	text t "* eightは第一引数の関数に8を適用する関数", \t -> do
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
	itext t 1 "*Main> let three = valToFun 3", \t -> do
	itext t 1 "*Main> three (+ 7)", \t -> do
	itext t 1 $ show $ three (+ 7), \t -> do
	itext t 1 "*Main> three even", \t -> do
	itext t 1 $ show $ three even, \t -> do
	itext t 1 "*Main> :load + Data.Char", \t -> do
	itext t 1 "*Main Data.Char> let c = valToFun 'c'", \t -> do
	itext t 1 "*Main Data.Char> c isLower", \t -> do
	itext t 1 $ show $ c isLower
 ]

funToVal :: (forall b . (a -> b) -> b) -> a
funToVal f = f id

valToFun5 :: Page
valToFun5 = [\t -> do
	writeTopTitle t "単純な値"
	text t "", \t -> do
	text t "* 単純な値から高階関数の形に変換する関数を作った", \t -> do
	text t "* 逆の方向の変換も可能である", \t -> do
	text t "* eight idで見たようにidをつけてやれば良い", \t -> do
	itext t 1 "fun :: (a -> b) -> b", \t -> do
	itext t 1 "val :: a", \t -> do
	itext t 1 "val = fun id"
 ]

valToFunSummary :: Page
valToFunSummary = [\t -> do
	writeTopTitle t "単純な値(まとめ)"
	text t "", \t -> do
	text t "* 以下の2つの型は同じものと考えられる", \t -> do
	itext t 1 "a", \t -> do
	itext t 1 "(a -> b) -> b", \t -> do
	text t "* 前者を後者の形式に変換する関数valToFunを定義した", \t -> do
	itext t 1 "valToFun :: a -> (a -> b) -> b", \t -> do
	itext t 1 "valToFun x f = f x", \t -> do
	text t "* 逆方向の変換は関数形式のほうに引数としてidを与える", \t -> do
	text t "* 以下のような形の関数はより単純な値に置き換え可", \t -> do
	itext t 1 "(Int -> b) -> b", \t -> do
	itext t 1 "(Char -> b) -> b", \t -> do
	itext t 1 "([Int] -> b) -> b", \t -> do
	itext t 1 "((Int -> Char) -> b) -> b"
 ]

myChr :: (a -> Int) -> (a -> Char)
myChr f = \x -> chr $ f x

aboutAddArg1 :: Page
aboutAddArg1 = [\t -> do
	writeTopTitle t "一引数関数"
	text t "", \t -> do
	text t "* 以下の関数を見てみよう", \t -> do
	itext t 1 "myChr :: (a -> Int) -> (a -> Char)", \t -> do
	itext t 1 "myChr f = \\x -> chr $ f x", \t -> do
	text t "* 上記と以下をtransFuns.hsに書き込もう", \t -> do
	itext t 1 "import Data.Char (chr, ord)", \t -> do
	text t "* 以下のように読める", \t -> do
	itext t 1 "- 関数fを引数にとり", \t -> do
	itext t 1 "- 「引数にfを適用しchrを適用する関数」を返す"
 ]

aboutAddArg2 :: Page
aboutAddArg2 = [\t -> do
	writeTopTitle t "一引数関数"
	text t "", \t -> do
	text t "* 使ってみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> myChr (* 2) 55", \t -> do
	itext t 1 $ show $ myChr (* 2) 55, \t -> do
	itext t 1 "*Main> myChr ord 'j'", \t -> do
	itext t 1 $ show $ myChr ord 'j', \t -> do
	itext t 1 "*Main> (chr . (* 2)) 55", \t -> do
	itext t 1 $ show $ (chr . (* 2)) 55, \t -> do
	itext t 1 "*Main> (chr . ord) 'j'", \t -> do
	itext t 1 $ show $ (chr . ord) 'j'
 ]

aboutAddArg3 :: Page
aboutAddArg3 = [\t -> do
	writeTopTitle t "一引数関数"
	text t "", \t -> do
	text t "* myChrの定義は以下のように書き換えられる", \t -> do
	itext t 1 "myChar :: (a -> Int) -> (a -> Char)"
	itext t 1 "myChar f = \\x -> chr $ f x", \t -> do
	arrowIText t 1 "myChar f = chr . f", \t -> do
	arrowIText t 1 "myChar = (chr .)", \t -> do
	text t "* myCharは関数chrと何かを合成する関数", \t -> do
	text t "* より一般的にすると", \t -> do
	itext t 1 "fun :: b -> c", \t -> do
	itext t 1 "fun' :: (a -> b) -> (a -> c)", \t -> do
	itext t 1 "fun' = (fun .)"
 ]

addArg :: (b -> c) -> ((a -> b) -> (a -> c))
addArg f = \g -> (\x -> f $ g x)

rmArg :: ((a -> b) -> (a -> c)) -> (b -> c)
rmArg f x = f (const x) undefined

aboutAddArg4 :: Page
aboutAddArg4 = [\t -> do
	writeTopTitle t "一引数関数"
	text t "", \t -> do
	text t "* (b -> c)の形から(a -> b) -> (a -> c)の形にする", \t -> do
	text t "* 以下のように考えることもできる", \t -> do
	itext t 1 "- 引数と返り値の両方に引数をひとつ追加する", \t -> do
	text t "* これを行う関数を作ろう", \t -> do
	itext t 1 "addArg :: (b -> c) -> ((a -> b) -> (a -> c))", \t -> do
	itext t 1 "addArg f = \\g -> (\\x -> f $ g x)", \t -> do
	text t "* これを変換していくと最終的には以下のようになる", \t -> do
	itext t 1 "addArg :: (b -> c) -> (a -> b) -> (a -> c)", \t -> do
	itext t 1 "addArg = (.)", \t -> do
	text t "* 単なる関数合成になる"
 ]

aboutAddArg5 :: Page
aboutAddArg5 = [\t -> do
	writeTopTitle t "一引数関数"
	text t "", \t -> do
	text t "* fとgの合成関数f . gは「gを適用した結果にfを適用する」", \t -> do
	text t "* 二引数関数としてみると(.)は関数合成する関数", \t -> do
	itext t 1 "(.) :: (b -> c) -> (a -> b) -> (a -> c)", \t -> do
	text t "* (.)を一引数関数としてみると", \t -> do
	itext t 1 "(.) :: (b -> c) -> ((a -> b) -> (a -> c))", \t -> do
	text t "* 「引数と返り値の両方に引数を1つ追加する関数」と読める"
 ]

aboutAddArg6 :: Page
aboutAddArg6 = [\t -> do
	writeTopTitle t "一引数関数"
	text t "", \t -> do
	text t "* 逆方向の変換をする関数を作成する", \t -> do
	itext t 1 "rmArg :: ((a -> b) -> (a -> c)) -> (b -> c)", \t -> do
	itext t 1 "rmArg f = \\x -> f (const x) undefined", \t -> do
	text t "* xはb型の値であり", \t -> do
	itext t 1 "- const x :: a -> b", \t -> do
	itext t 1 "- f (const x) :: a -> c", \t -> do
	itext t 1 "- f (const x) undefined :: c"
 ]

aboutAddArgSummary :: Page
aboutAddArgSummary = [\t -> do
	writeTopTitle t "一引数関数(まとめ)"
	text t "", \t -> do
	text t "* 以下の2つの関数はほとんど同じものと考えることができる", \t -> do
	itext t 1 "fun :: b -> c", \t -> do
	itext t 1 "fun' :: (a -> b) -> (a -> c)", \t -> do
	itext t 1 "fun' = (fun .)", \t -> do
	text t "* 「引数と返り値の両方に1つ引数を追加する」変換", \t -> do
	text t "* それぞれの変換を行う関数addArg, rmArgを定義した", \t -> do
	text t "* addArgは関数合成(.)と同じものだった", \t -> do
	text t "* 以下のような形の型を見たらより単純な関数に置き換え可", \t -> do
	itext t 1 "(a -> Int) -> (a -> Char)", \t -> do
	itext t 1 "(a -> [Int]) -> (a -> [Bool])", \t -> do
	itext t 1 "(a -> Int -> Char) -> (a -> Bool)"
 ]

myAdd :: (a -> Int) -> Int -> (a -> Int)
myAdd f y = \x -> f x + y

aboutAddArg21 :: Page
aboutAddArg21 = [\t -> do
	writeTopTitle t "二引数関数"
	text t "", \t -> do
	text t "* 以下の関数を考える", \t -> do
	itext t 1 "myAdd :: (a -> Int) -> Int -> (a -> Int)", \t -> do
	itext t 1 "myAdd f y = \\x -> f x + y", \t -> do
	text t "* 関数と整数を取り"
	itext t 1 "「引数に関数を適用した結果に整数を足す」関数を返す", \t -> do
	text t "* transFuns.hsに書き込み、:reloadする", \t -> do
	itext t 1 "*Main> (myAdd (* 2) 3) 8", \t -> do
	itext t 1 $ show $ (myAdd (* 2) 3) 8
 ]

aboutAddArg22 :: Page
aboutAddArg22 = [\t -> do
	writeTopTitle t "二引数関数"
	text t "", \t -> do
	text t "* より一般的には以下のようになる", \t -> do
	itext t 1 "fun :: b -> c -> d", \t -> do
	itext t 1 "fun' :: (a -> b) -> c -> (a -> d)", \t -> do
	itext t 1 "fun' f y = \\x -> fun (f x) y", \t -> do
	text t "* 「第一引数と返り値に引数を1つ追加する」変換", \t -> do
	itext t 0 "addArg2 :: (b -> c -> d) -> (a -> b) -> c -> (a -> d)"
	itext t 0 "addArg2 fun f y = \\x -> fun (f x) y", \t -> do
	text t "* これをポイントフリースタイルにすると", \t -> do
	itext t 1 "addArg2 = (. flip (.)) . flip (.) . flip", \t -> do
	text t "* この変換には追加の情報は必要ないことがわかる"
 ]

rmArg2 :: ((a -> b) -> c -> (a -> d)) -> (b -> c -> d)
rmArg2 f x y = f (const x) y undefined

aboutAddArg23 :: Page
aboutAddArg23 = [\t -> do
	writeTopTitle t "二引数関数"
	text t "", \t -> do
	text t "* 逆方向の変換", \t -> do
	itext t 0 "rmArg2 :: ((a -> b) -> c -> (a -> d)) -> (b -> c -> d)"
	itext t 0 "rmArg2 f = \\x y -> f (const x) y undefined", \t -> do
	text t "* x, yの型はそれぞれb, c", \t -> do
	itext t 1 "- const x :: a -> b", \t -> do
	itext t 1 "- f (const x) :: c -> (a -> d)", \t -> do
	itext t 1 "- f (const x) y :: a -> d", \t -> do
	itext t 1 "- f (const x) y undefined :: d"
 ]

aboutAddArg2Summary :: Page
aboutAddArg2Summary = [\t -> do
	writeTopTitle t "二引数関数(まとめ)"
	text t "", \t -> do
	text t "* 以下の2つの関数はほぼ同じ", \t -> do
	itext t 1 "fun :: b -> c -> d", \t -> do
	itext t 1 "fun' :: (a -> b) -> c -> (a -> d)", \t -> do
	itext t 1 "fun' f y = \\x -> fun (f x) y", \t -> do
	text t "* 第一引数と返り値に引数を1つ追加する関数", \t -> do
	itext t 1 "addArg2 fun f y = \\x -> fun (f x) y", \t -> do
	text t "* 以下の型を見たらより簡単な型に変換可", \t -> do
	itext t 1 "(a -> Char) -> Int -> (a -> Bool) ", \t -> do
	itext t 1 "(a -> [Bool]) -> [Char] -> (a -> [Int])", \t -> do
	itext t 1 "(a -> Char) -> (Double -> Bool) -> (a -> Int)"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ", \t -> do
	text t "* 意味がほとんど変化しない変換を見てきた", \t -> do
	text t "* これらは片方が存在すればもう一方を導ける", \t -> do
	text t "* 複雑なほうの型の関数を作る場合", \t -> do
	itext t 1 "- より簡単なほうの型の関数から変換するほうが", \t -> do
	itext t 1 "- コードがすっきリするだろう", \t -> do
	text t "* 以下の変換を次回の「モナド」の回で使う", \t -> do
	itext t 1 "b -> c -> d", \t -> do
	arrowIText t 1 "(a -> b) -> c -> (a -> d)", \t -> do
	text t "* これは後者の形の関数が必要になった場合", \t -> do
	itext t 1 "前者の形の関数から導出できるということを意味する", \t -> do
	text t "* 引数と結果の関数に引数として同じ型変数がある場合", \t -> do
	itext t 1 "- それを削除できる、と考えておこう"
 ]
