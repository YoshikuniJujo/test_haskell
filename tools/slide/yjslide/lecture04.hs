module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第4回 リストとそれを扱う関数"

pages :: [Page]
pages = [
	titlePage, noLoop,
	makeList, makeList2,
	mapZipWith, filterFun, foldrFun, dollar, dot,
	zoromeDef,
	zoromeAnswer1, zoromeAns1Pred, zoromeAns1List, zoromeAns1Sum,
	zoromeAnswer2Pre, zoromeAnswer2, zoromeAns2Ones, zoromeAns2List,
	zoromeAns2List2,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

noLoop :: Page
noLoop = [\t -> do
	writeTopTitle t "繰り返し"
	text t "ループもない状態変化もない"
	arrowIText t 1 "一体どうすればいい?"
	text t "", \t -> do
	text t "リストを作る", \t -> do
	arrowIText t 1 "map, filter でリストを操作", \t -> do
	arrowIText t 1 "foldr でまとめる", \t -> do
	dvArrow t
	text t "リストと高階関数を駆使"
 ]

makeList :: Page
makeList = [\t -> do
	writeTopTitle t "リストの生成", \t -> do
	semititle t "enumFrom関数", \t -> do
	text t "指定した値から始まる無限リストを作る", \t -> do
	text t "例: enumFrom 3 => [3, 4, 5, ...]", \t -> do
	semititle t "enumFromTo関数", \t -> do
	text t "始めと終わりの値を指定することでリストを作る", \t -> do
	text t "例: enumFromTo 3 8 => [3, 4, 5, 6, 7, 8]"
	text t "", \t -> do
	arrowIText t 0 "上記2つの関数には構文糖が用意されていて"
	itext t 1 "[3 ..]や[3 .. 8]のように書ける"
 ]

makeList2 :: Page
makeList2 = [\t -> do
	writeTopTitle t "リストの生成", \t -> do
	semititle t "iterate関数", \t -> do
	text t "指定した関数を0, 1, 2 ... 回適用した無限リストを作る"
	text t "iterate f x => [x, f x, f (f x), f (f (f x)), ...]"
	text t "", \t -> do
	text t "例: iterate (* 2) 1 => [1, 2, 4, 8, 16, ...]"
 ]

mapZipWith :: Page
mapZipWith = [\t -> do
	writeTopTitle t "map", \t -> do
	semititle t "map関数", \t -> do
	text t "関数をリストの要素すべてに適用する"
	text t "map f [x1, x2, x3 ...] => [f x1, f x2, f x3 ...]", \t -> do
	semititle t "zipWith関数", \t -> do
	text t "2引数関数を2つのリストのそれぞれの要素に適用する"
	text t "zipWith f [x1, x2 ...] [y1, y2 ...]"
	itext t 1 "=> [f x1 y1, f x2 y2 ...]", \t -> do
	semititle t "concatMap関数", \t -> do
	text t "concatMap f list == concat (map f list)", \t -> do
	text t "concat [[1,2],[3,4],[5,6,7]] => [1,2,3,4,5,6,7]", \t -> do
	text t "mapによって作られたリストのリストを平らにする"
 ]

filterFun :: Page
filterFun = [\t -> do
	writeTopTitle t "filter", \t -> do
	semititle t "filter関数", \t -> do
	text t "filter p xs で"
	text t "p x の値がTrueのものだけ集めたリストを返す", \t -> do
	text t "例: filter even [1, 2, 3, 4, 5] => [2, 4]"
	text t "", \t -> do
	semititle t "takeWhile関数", \t -> do
	text t "takeWhile p xs で"
	text t "p x の値がTrueであるあいだだけリストの値を取り出す", \t -> do
	text t "例: takeWhile (< 5) [2, 3, 5, 4, 3] => [2, 3]"
 ]

foldrFun :: Page
foldrFun = [\t -> do
	writeTopTitle t "foldr", \t -> do
	semititle t "foldr関数", \t -> do
	text t "演算子を右結合でリストの要素に次々と適用していく"
	text t "foldr op x0 [x1, x2, x3 ...] =>"
	itext t 1 "x1 `op` (x2 `op` (x3 `op` ... `op` x0)...))", \t -> do
	text t "x1 : (x2 : (x3 : []))で(:)をopに[]をx0に置き換える", \t -> do
	semititle t "sum関数", \t -> do
	text t "sum = foldr (+) 0", \t -> do
	text t "x1 + (x2 + (x3 + 0))", \t -> do
	semititle t "and関数", \t -> do
	text t "and = foldr (&&) True", \t -> do
	text t "x1 && (x2 && (x3 && True))"
 ]

dollar :: Page
dollar = [\t -> do
	writeTopTitle t "$演算子"
	text t "", \t -> do
	text t "* 関数適用を意味する演算子", \t -> do
	text t "* f $ xでfにxを入力するということ", \t -> do
	text t "* f xと同じ", \t -> do
	text t "* 優先順位が低く作られている", \t -> do
	text t "* ()を省略することができる"
	text t "", \t -> do
	text t "例: f (g (h x)) -> f $ g $ h x"
 ]

dot :: Page
dot = [\t -> do
	writeTopTitle t ".演算子"
	text t "", \t -> do
	text t "* 関数を結合する演算子", \t -> do
	text t "* (f . g) x = f (g x)", \t -> do
	text t "* xにgを適用してその結果にfを適用するということ"
	text t "", \t -> do
	text t "例: (* 3) . (+ 2)"
	itext t 1 "2を足して3をかける関数"
 ]

zoromeDef :: Page
zoromeDef = [\t -> do
	writeTopTitle t "例題"
	text t "", \t -> do
	semititle t "* 0から与えられた数までのぞろ目の和を求める", \t -> do
	semititle t "* ぞろ目とは333や777などの同じ数字の連続", \t -> do
	semititle t "* 1, 2, 3 ... 9といった1桁の数もぞろ目と考える"
	text t "", \t -> do
	text t "この例題について考えていこう"
 ]

za1p, za1l, za1s, za2o, za2l :: [String]
za1p = [
	"isZorome :: String -> Bool",
	"isZorome ds = and $ zipWith (==) ds (tail ds)",
	""]
za1l = [
	"zoromes :: [Integer]",
	"zoromes = filter (isZorome . show) [1 ..]",
	""]
za1s = [
	"sumZoromes :: Integer -> Integer",
	"sumZoromes n = sum $ takeWhile (<= n) zoromes",
	""]
za2o = [
	"ones :: [Integer]",
	"ones = iterate ((+ 1) . (* 10)) 1",
	""]
za2l = [
	"zoromes :: [Integer]",
	"zoromes = concatMap (\\o -> map (o *) [1 .. 9]) ones",
	""]

zoromeAnswer1 :: Page
zoromeAnswer1 = [\t -> do
	writeTopTitle t "解1", \t -> do
	mapM_ (text t) za1p
	mapM_ (text t) za1l
	mapM_ (text t) za1s, \t -> do
	text t "すべての自然数から成るリストを作成", \t -> do
	arrowIText t 1 "isZoromeでfilterしぞろ目だけのリストを作る", \t -> do
	arrowIText t 1 "n以下までを合計する"
 ]

zoromeAns1Pred :: Page
zoromeAns1Pred = [\t -> do
	writeTopTitle t "解1(解説)"
	text t "", \t -> do
	mapM_ (text t) za1p, \t -> do
	text t "* Stringは[Char]の別名", \t -> do
	text t "* すべての要素が同じ", \t -> do
	arrowIText t 1 "ひとつずらしたリストとの等値比較がすべてTrue", \t -> do
	text t "* tailはリストの先頭を消したものを返す", \t -> do
	text t "* zipWithで同じ位置の要素同士を比較"
	arrowIText t 1 "Bool値のリストが返る", \t -> do
	text t "* andによって全てTrueであることを確認する"
 ]

zoromeAns1List :: Page
zoromeAns1List = [\t -> do
	writeTopTitle t "解1(解説)"
	text t "", \t -> do
	mapM_ (text t) za1l, \t -> do
	text t "* [1 ..]は[1, 2, 3, 4, ...]ということ"
	itext t 1 "つまりすべての自然数", \t -> do
	text t "* それぞれ表記をshowで求めてisZoromeでfilterする"
 ]

zoromeAns1Sum :: Page
zoromeAns1Sum = [\t -> do
	writeTopTitle t "解1(解説)"
	text t "", \t -> do
	mapM_ (text t) za1s, \t -> do
	text t "* n以下の要素を取り出す"
	text t "* 総和を求める"
 ]

zoromeAnswer2Pre :: Page
zoromeAnswer2Pre = [\t -> do
	writeTopTitle t "解2"
	text t "* 解1は非効率"
	itext t 1 "わずかなぞろ目を探すためにすべての整数をチェック", \t -> do
	text t "* ぞろ目は以下の特徴を持つ"
	itext t 1 "- 1のぞろ目に1から9までの整数をかけたもの"
	itext t 1 "- 1のぞろ目は1 + 10 + 100 + 100 + ...", \t -> do
	text t "* 上記性質を利用すればぞろ目のみのリストを生成できる", \t -> do
	text t "* sumZorome は変えなくて良い"
	itext t 1 "数え上げと和を求めるロジックがきれいに分離"
 ]

zoromeAnswer2 :: Page
zoromeAnswer2 = [\t -> do
	writeTopTitle t "解2"
	text t ""
	mapM_ (text t) za2o
	mapM_ (text t) za2l
	mapM_ (text t) za1s, \t -> do
	text t "1のぞろ目のリストを作る", \t -> do
	arrowIText t 1 "それぞれに1から9をかけてそれらを結合する"
 ]

zoromeAns2Ones :: Page
zoromeAns2Ones = [\t -> do
	writeTopTitle t "解2(解説)"
	text t ""
	mapM_ (text t) za2o, \t -> do
	text t "* 演算子の部分適用"
	itext t 1 "(+ 1) は \\x -> x + 1 ということ", \t -> do
	text t "* (+ 1) . (* 10) は10かけて1を足すということ"
	itext t 1 "つまり1 -> 11, 11 -> 111, 111 -> 1111となる"
 ]

zoromeAns2List :: Page
zoromeAns2List = [\t -> do
	writeTopTitle t "解2(解説)"
	text t ""
	mapM_ (text t) za2l, \t -> do
	text t "* concatMapをばらして考える"
	itext t 1 "concat $ map (\\o -> map (o *) [1 .. 9]) ones", \t -> do
	arrowIText t 1 "concat [[1,2...], [11,22...], [111,...], ...]", \t -> do
	arrowIText t 1 "[1,2,...,11,22,...,111,...]"
 ]

zoromeAns2List2 :: Page
zoromeAns2List2 = [\t -> do
	writeTopTitle t "リスト内包表記"
	text t ""
	mapM_ (text t) za2l, \t -> do
	text t "* 上記のようなパターンはよく使われる"
	itext t 1 "xsとysの要素のすべての組み合わせについて処理する", \t -> do
	arrowIText t 1 "特別な表記法が用意されている"
	itext t 2 "[f x y | x <- xs, y <- ys] のような形", \t -> do
	text t "* リスト内包表記と呼ぶ", \t -> do
	text t "* これを使って書き換えると"
	itext t 1 "[o * n | o <- ones, n <- [1 .. 9]]"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* ループの多くはリストとそれを扱う関数で実現できる", \t -> do
	text t "* Haskellにはリストを扱う関数が多数用意されている", \t -> do
	text t "* 今回紹介しきれなかった多くの便利な関数がある", \t -> do
	text t "* 上記の関数群は次回学ぶ再帰的定義を使って定義される", \t -> do
	text t "* 次回は再帰的定義を学ぶ", \t -> do
	text t "* しかし、素の再帰を使うのは最後の手段と考えるべき", \t -> do
	text t "* まずはリストと既存の関数で問題解決することを考える"
 ]

zoromeAns2Ones2 :: Page
zoromeAns2Ones2 = [\t -> do
	writeTopTitle t "解2(解説)"
	text t ""
	text t "ones の要素を0番目から順に展開していってみよう", \t -> do
	text t "ones", \t -> do
	arrowIText t 1 "1 : map ((+ 1) . (* 10)) ones", \t -> do
	arrowIText t 1 "1 : map ((+ 1) . (* 10))"
	itext t 2 "(1 : map ((+ 1) . (* 10)) ones)", \t -> do
	arrowIText t 1 "1 : 11 : map ((+ 1) . (* 10))"
	itext t 2 "(map ((+ 1) . (* 10)) ones)"
 ]

others1 :: Page
others1 = [\t -> do
	text t "* isZorome は文字列が同じ文字のみから成っていればTrue", \t -> do
	text t "* zoromes はすべてのぞろ目から成るリスト", \t -> do
	text t "* sumZoromes はn以下のぞろ目すべての和となる"
 ]

others :: Page
others = [\t -> do
	text t "* [0 ..]は0以上のすべての整数から成るリスト", \t -> do
	text t "* zoromesは上記リストをisZoromeでfilterしたもの", \t -> do
	text t "* .演算子は (f . g) x = f (g x) と定義される", \t -> do
	text t "* takeWhileはリストが条件を満たすあいだだけ要素を取る", \t -> do
	text t "* sumはリストに含まれる数の和を返す"
 ]
