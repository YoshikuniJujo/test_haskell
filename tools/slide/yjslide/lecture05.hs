module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第5回 再帰関数"

pages :: [Page]
pages = [
	titlePage, prelude, simple, simpleDevelop, summary1,
	iterative2, iterative3, iterative4,
	recursiveList, recursiveList2,
	fibonacci1, fibonacci2, fibonacci3, fibonacci4, fibonacci5,
	fibonacci6, fibonacciSummary,
	functions0,
	functions1 1, lengthFun,
	functions1 2, sumFun,
	functions1 3, mapFun,
	functions1 4, filterFun,
	functions1 5, foldrFun,
	functions1 6, foldlFun,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに", \t -> do
	semititle t "* リストを扱う関数は再帰的に定義されている", \t -> do
	semititle t "* 再帰関数を学ぶことで", \t -> do
	itext t 1 "それらの関数の中身を知ることができる", \t -> do
	itext t 1 "より広い範囲の関数を定義することができる", \t -> do
	semititle t "* 既存の関数が使えるときはそっちを使うべき", \t -> do
	itext t 1 "ただし「学習のため」や「とりあえず作ってみる」"
	itext t 1 "といった場合は再帰関数を作ることもある", \t -> do
	semititle t "* 直接、再帰を使わないと表現できないことも", \t -> do
	itext t 1 "が、そういう場面は稀である"
 ]

simple :: Page
simple = [\t -> do
	writeTopTitle t "簡単な例"
	text t "0からnまでの自然数の総和を求める関数"
	text t "", \t -> do
	text t "sumTo :: Integer -> Integer"
	text t "sumTo 0 = 0"
	text t "sumTo n = n + sumTo (n - 1)"
	text t "", \t -> do
	text t "これは"
	itext t 1 "0から0までの和は0であり、"
	itext t 1 "0からnまでの和は0からn - 1までの和にnを足したもの"
	text t "と読める"
	text t "", \t -> do
	arrowIText t 0 "性質を定義している"
 ]

simpleDevelop :: Page
simpleDevelop = [\t -> writeTopTitle t "展開してみる"] ++ sumToDevelop

sumToDevelop :: Page
sumToDevelop = [\t ->
	text t "", \t -> do
	itext t 1 "sumTo 3", \t -> do
	arrowIText t 2 "3 + sumTo (3 - 1)", \t -> do
	arrowIText t 2 "3 + sumTo 2", \t -> do
	arrowIText t 2 "3 + (2 + sumTo (2 - 1))", \t -> do
	arrowIText t 2 "3 + (2 + sumTo 1)", \t -> do
	arrowIText t 2 "3 + (2 + (1 + sumTo (1 - 1)))", \t -> do
	arrowIText t 2 "3 + (2 + (1 + sumTo 0))", \t -> do
	arrowIText t 2 "3 + (2 + (1 + 0))", \t -> do
	arrowIText t 2 "6"
 ]

summary1 :: Page
summary1 = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	semititle t "* 大事なのはここ", \t -> do
	itext t 1 "k = 0の値を定義", \t -> do
	itext t 1 "k = nの値をk = n - 1の値を使って定義", \t -> do
	dvArrowShort t
	itext t 1 "すべての値が定義される"
 ]

iterative1 :: Page
iterative1 = [\t -> do
	writeTopTitle t "反復的プロセス", \t -> do
	text t "もう一度さっきの展開の様子を見てみよう", \t ->
 	mapM_ ($ t) sumToDevelop
 ]

iterative2 :: Page
iterative2 = [\t -> do
	writeTopTitle t "反復的プロセス"
	text t "", \t -> do
	text t "再帰関数によって表現されるプロセスには大きく分けて"
	itext t 1 "* 再帰的プロセス"
	itext t 1 "* 反復的プロセス"
	text t "の2つがある", \t -> do
	semititle t "再帰的プロセス", \t -> do
	text t "sumTo n = n + sumTo (n - 1) は再帰的プロセスとなる", \t -> do
	text t "sumTo (n - 1) の実行中に文脈 n + ... の記憶が必要", \t -> do
	semititle t "反復的プロセス", \t -> do
	text t "次に呼び出されるプロセスは何も覚えておかなくて良い"
 ]

iterative3 :: Page
iterative3 = [\t -> do
	writeTopTitle t "反復的プロセス"
	text t "", \t -> do
	text t "sumToIter :: Integer -> Integer -> Integer"
	text t "sumToIter s 0 = s"
	text t "sumToIter s n = sumToIter (s + n) (n - 1)"
	text t ""
	text t "sumToI :: Integer -> Integer"
	text t "sumToI = sumToIter 0"
	text t "", \t -> do
	text t "* sumToIterの第2引数はnから0まで動く", \t -> do
	text t "* そのなかで、その和は第1引数sに蓄積され、"
	itext t 1 "第2引数が0に到達した時点で返される", \t -> do
	text t "* sumToIはsの初期値0を与える"
 ]

iterative4 :: Page
iterative4 = [\t -> do
	writeTopTitle t "反復的プロセス(まとめ)"
	text t "", \t -> do
	semititle t "* 関数は次に呼び出される関数に置き換えられる"
	itext t 1 "呼び出しの文脈を覚えておく必要はない", \t -> do
	semititle t "* 状態を保存するための追加の変数が必要になる", \t -> do
	semititle t "* 空間効率(メモリ使用量)を改善する鍵となる"
	itext t 1 "Haskellでは正格評価のための仕組みが必要となる", \t -> do
	semititle t "* 再帰的プロセスと反復的プロセスの使い分け"
	itext t 1 "おおまかには「わかりやすさ」の再帰的プロセスと"
	itext t 1 "「効率」の反復的プロセスと考えておく"
 ]

recursiveList :: Page
recursiveList = [\t -> do
	writeTopTitle t "リストの再帰的定義"
	text t "", \t -> do
	text t "簡単な例"
	text t "ones :: [Integer]"
	text t "ones = 1 : ones"
	text t "", \t -> do
	text t "展開してみる"
	itext t 1 "ones"
	arrowIText t 2 "1 : ones"
	arrowIText t 2 "1 : (1 : ones)"
	arrowIText t 2 "1 : (1 : (1 : ones))"
	text t "つまり、[1, 1, 1, 1 ...]ということ"
 ]

recursiveList2 :: Page
recursiveList2 = [\t -> do
	writeTopTitle t "リストの再帰的定義", \t -> do
	text t "もうすこし複雑な例"
	itext t 1 "ints :: [Integer]"
	itext t 1 "ints = 1 : map (+ 1) ints"
	text t "", \t -> do
	text t "展開してみる"
	arrowIText t 1 "1 : map (+ 1) ints", \t -> do
	arrowIText t 1 "1 : map (+ 1) (1 : map (+ 1) ints)", \t -> do
	arrowIText t 1 "1 : (1 + 1) : map (+ 1) (map (+ 1) ints)", \t -> do
	arrowIText t 1 "1 : 2 : map (+ 1) (map (+ 1) (1 : map (+ 1) ints))", \t -> do
	arrowIText t 1 "1 : 2 : 3 : ..."
 ]

fibTitle :: String
fibTitle = "フィボナッチ数列"

fibonacci1 :: Page
fibonacci1 = [\t -> do
	writeTopTitle t fibTitle
	text t "例としてフィボナッチ数列について見ていこう"
	text t "", \t -> do
	semititle t "フィボナッチ数列とは?", \t -> do
	text t "直前の2つの数の和が次の数となるような数列", \t -> do
	text t "数学的に表記するとこうなる"
	itext' t 1 "x_{0} = 0"
	itext' t 1 "x_{1} = 1"
	itext' t 1 "x_{n+2} = x_{n+1} + x_{n}", \t -> do
	text t "0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144 ..."
	text t "ということ"
 ]

fibonacci2 :: Page
fibonacci2 = [\t -> do
	writeTopTitle t fibTitle
	text t "", \t -> do
	text t "同じことをHaskellで定義してみる", \t -> do
	itext t 1 "fib 0 = 0"
	itext t 1 "fib 1 = 1"
	itext t 1 "fib n = fib (n - 1) + fib (n - 2)", \t -> do
	text t "いいね!"
	preLine t
	itext t 3 "わかりやすい!"
	itext t 2 "ちゃんと動くよ!"
	text t "fib 5 => 5"
	text t "fib 10 => 55"
	text t "fib 20 => 6765"
	text t "fib 30 => 832040"
	text t "fib 40 => あれっ? 遅っ!!"
 ]

fibonacci3 :: Page
fibonacci3 = [\t -> do
	writeTopTitle t fibTitle
	text t "", \t -> do
	text t "なんで、こんなに遅いの?"
	text t "フィボナッチ数列の40番目ならせいぜい40回の足し算のはず", \t -> do
	dvArrowShort t
	text t "本当は1億6558万140回の足し算をしている"
	text t "", \t -> do
	text t "fib n = fib (n - 1) + fib (n - 2)をよく見ると"
	text t "fib (n - 1)を求めるときfib (n - 2)の計算がダブっている"
	text t "", \t -> do
	text t "計算量はだいたい2倍2倍で増加している", \t -> do
	text t "つまりO(2^n)の時間と空間が必要となる"
 ]

fibonacci4 :: Page
fibonacci4 = [\t -> do
	writeTopTitle t fibTitle, \t -> do
	text t "* 効率を改善しよう", \t -> do
	itext t 1 "メモ化してもいいけど...", \t -> do
	itext t 1 "今回は反復的プロセスを使ってみよう"
	text t "", \t -> do
	text t "反復的プロセスにするにはどうしたら良いか"
	arrowIText t 1 "関数の引数にすべての情報を乗っければ良い", \t -> do
	text t "n番目を求めるにはn-1番目とn-2番目の値が必要", \t -> do
	itext t 1 "fibIter a _ 0 = a"
	itext t 1 "fibIter a b n = fib b (a + b) (n - 1)"
	itext t 1 "fibI = fibIter 0 1", \t -> do
	text t "n-1番目とn番目からn番目とn+1番目を求めている"
 ]

fibonacci5 :: Page
fibonacci5 = [\t -> do
	writeTopTitle t fibTitle
	text t "展開してみよう"
	text t "", \t -> do
	text t "fibIter 0 1 6", \t -> do
	arrowIText t 1 "fibIter 1 1 5", \t -> do
	arrowIText t 1 "fibIter 1 2 4", \t -> do
	arrowIText t 1 "fibIter 2 3 3", \t -> do
	arrowIText t 1 "fibIter 3 5 2", \t -> do
	arrowIText t 1 "fibIter 5 8 1", \t -> do
	arrowIText t 1 "fibIter 8 13 0", \t -> do
	arrowIText t 1 "8"
 ]

fibonacci6 :: Page
fibonacci6 = [\t -> do
	writeTopTitle t fibTitle
	text t "今度はリストの再帰的な定義を使ってみよう"
	text t "", \t -> do
	text t "下図のように考える", \t -> do
	itext t 1.8 "fibonacci = 0 : 1 : 1 : 2 : 3 : ..."
	itext t 4 "+"
	itext t 1 "tail fibonacci = 1 : 1 : 2 : 3 : 5 : ..."
	hLine t 20 65
	itext t 0.5 "fibonacci = 0 : 1 : 1 : 2 : 3 : 5 : 8 : ...", \t -> do
	dvArrowShort t
	text t "fibonacci = 0 : 1 :"
	itext t 1 "zipWith (+) fibonacci (tail fibonacci)"
 ]

fibonacciSummary :: Page
fibonacciSummary = [\t -> do
	writeTopTitle t "フィボナッチ数列(まとめ)"
	text t "", \t -> do
	text t "* 素直な実装は遅い", \t -> do
	text t "* 反復的プロセスに変換すると早くなる", \t -> do
	text t "* 無限リストを利用すると「数列」の本質を表現可能"
 ]

functions :: [String]
functions = ["length", "sum", "map", "filter", "foldr", "foldl"]

showFunction :: Int -> Int -> String -> Turtle -> IO ()
showFunction n i f = \t ->
	(if n == i then withRed t else id) $ semititle t $ show i ++ ". " ++ f

functions0 :: Page
functions0 = [\t -> writeTopTitle t "基本的な関数の定義" >> text t ""] ++
		(zipWith (showFunction 0) [1 ..] functions) ++
		[\t -> text t "" >> text t "これらの関数の定義を見ていこう"]

functions1 :: Int -> Page
functions1 n = [\t -> oneshot t $ do
	writeTopTitle t "基本的な関数の定義"
	oneshot t $ do
		text t ""
		mapM_ ($ t) $ (zipWith (showFunction n) [1 ..] functions)]

lengthFun :: Page
lengthFun = [\t -> do
	writeTopTitle t "length"
	text t "", \t -> do
	text t "length :: [a] -> Int"
	text t "length [] = 0"
	text t "length (_ : xs) = 1 + length xs"
	text t "", \t -> do
	text t "空リストの長さは0", \t -> do
	text t "xsに何かを加えたリストはxsの長さに1を足した長さ"
 ]

sumFun :: Page
sumFun = [\t -> do
	writeTopTitle t "sum"
	text t "", \t -> do
	text t "sum :: Num a => [a] -> a"
	text t "sum [] = 0"
	text t "sum (x : xs) = x + sum xs"
	text t "", \t -> do
	text t "Num a => はクラス制約"
	itext t 1 "クラス制約は第7回あたりでやる「型クラス」で"
	text t "空リストの内の値の合計は0"
	text t "xとxsの合計の和が全体の合計となる"
 ]

mapFun :: Page
mapFun = [\t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "map :: (a -> b) -> [a] -> [b]"
	text t "map _ [] = []"
	text t "map f (x : xs) = f x : map f xs"
	text t "", \t -> do
	text t "空リストのすべての要素に関数を適用した結果は空リスト", \t -> do
	text t "xに関数を適用した結果をxsのすべての要素に"
	itext t 1 "関数を適用した結果の頭に足したものは"
	itext t 1 "x : xs のすべての要素に関数を適用したもの"
 ]

filterFun :: Page
filterFun = [\t -> do
	writeTopTitle t "filter"
	text t "", \t -> do
	text t "filter :: (a -> Bool) -> [a] -> [a]"
	text t "filter _ [] = []"
	text t "filter p (x : xs)"
	itext t 1 "| p x = x : filter p xs"
	itext t 1 "| otherwise = filter p xs"
	text t "", \t -> do
	text t "ガードについてはどこかで触れる必要があるな -> 自分"
	text t "空リストから何を抽出しようとしても空リスト"
	text t "もしもxが条件を満たすならxsのうちの条件を満たすものの"
	itext t 1 "リストにxを足したもの"
	text t "条件を満たさないならxsのうちの条件を満たすものを集めたもの"
 ]

foldrFun :: Page
foldrFun = [\t -> do
	writeTopTitle t "foldr"
	text t "", \t -> do
	text t "foldr :: (a -> b -> b) -> b -> [a] -> b"
	text t "foldr _ v [] = v "
	text t "foldr op v (x : xs) = x `op` foldr op v xs"
	text t "", \t -> do
	text t "空リストはvになる"
	text t "x : xsの':'がopで置き換えられる"
	text t "", \t -> do
	text t "リストを扱う再帰的プロセスの枠組を抽象化している"
 ]

foldlFun :: Page
foldlFun = [\t -> do
	writeTopTitle t "foldl"
	text t "", \t -> do
	text t "foldl :: (a -> b -> a) -> a -> [b] -> a"
	text t "foldl _ v [] = v"
	text t "foldl op v (x : xs) = foldl op (v `op` x) xs"
	text t "", \t -> do
	text t "foldlは左結合で要素に演算子を適用していく関数"
	text t "vにxを演算子で結合したものがvに蓄積されていく"
	text t "", \t -> do
	text t "リストに対する反復的プロセスの枠組を抽象化している"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 疲れてきた", \t -> do
	text t "* 再帰関数の定義のしかたについて学んだ", \t -> do
	text t "* 大きく分けて再帰的プロセスと反復的プロセスがある", \t -> do
	text t "* リストを扱う再帰関数についても学んだ", \t -> do
	text t "* 疲れてきてすこしよくわからなくなっている", \t -> do
	text t "* すこし休んだほうが良さそうだ", \t -> do
	text t "* 第6回にIOについてやる", \t -> do
	text t "* 第7回に型クラスについてやる", \t -> do
	text t "* 第8回にmonadについてやる", \t -> do
	text t "* template haskell, iteratee とかもやりたい"
 ]
