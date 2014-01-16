import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第6回 再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutSquareSum, aboutSquareSum2, aboutSquareSum3, aboutSquareSum4,
	aboutSquareSum5, aboutSquareSum6, aboutSquareSum7, aboutSquareSum8,
	aboutSquareSum9, aboutSquareSum10, aboutSquareSum11, aboutSquareSumSummary,
	treeRec
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 再帰関数は関数型言語の魂", \t -> do
	text t "* 「くりかえし」を含むさまざまな処理の流れが表現可能", \t -> do
	text t "* 前回のリストを処理する関数群も再帰的に定義されている", \t -> do
	text t "* 非常に強力な道具", \t -> do
	text t "* 再帰関数を使えば、ほとんど「何でもできる」が", \t -> do
	text t "* 直接的に再帰関数を使う前に", \t -> do
	text t "* より力の弱い方法で実現できないか考えたほうがいい", \t -> do
	itext t 1 "- 再帰を使うのではなくmapとfilterを使う、等", \t -> do
	text t "* そのほうがプログラムがわかりやすくなる場合がある"
 ]

aboutSquareSum :: Page
aboutSquareSum = [\t -> do
	writeTopTitle t "二乗の和"
	text t "", \t -> do
	text t "* 「リスト」の回でやった二乗の和を再帰で見てみよう", \t -> do
	text t "* 前回のやりかたのほうがプログラムはわかりやすいので", \t -> do
	itext t 1 "この例の場合は前回のようにリストを使うほうが良い", \t -> do
	text t "* 今回のやりかたを見ることによって", \t -> do
	itext t 1 "- より低レベルの「仕組み」が理解できる", \t -> do
	itext t 1 "- より広い範囲に応用できる"
 ]

aboutSquareSum2 :: Page
aboutSquareSum2 = [\t -> do
	writeTopTitle t "問題定義"
	text t "", \t -> do
	text t "* 与えられた引数nに対して0からnまでの二乗の和を求める", \t -> do
	text t "* リストを使った解", \t -> do
	itext t 1 "1. 0からnまでのリストがあるとする", \t -> do
	itext t 1 "2. そのすべての要素を二乗したリストがあれば", \t -> do
	itext t 1 "3. そのリストの要素の総和が求める値である", \t -> do
	text t "* 直接的に再帰を使った解", \t -> do
	itext t 1 "1. 0から0までの二乗の和は0である", \t -> do
	itext t 1 "2. 0から(n - 1)までの二乗の和Sがあれば", \t -> do
	itext t 1 "3. 0からnまでの二乗の和はn ^ 2 + S"
 ]

aboutSquareSum3 :: Page
aboutSquareSum3 = [\t -> do
	writeTopTitle t "関数定義"
	text t "", \t -> do
	text t "* 1. 0から0までの二乗の和は0である", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	text t "* 2. 0から(n - 1)までの二乗の和Sがあれば", \t -> do
	itext t 1 "SはsquareSum (n - 1)", \t -> do
	text t "* 3. 0からnまでの二乗の和はn ^ 2 + S", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	text t "* よって以下のようになる", \t -> do
	itext t 1 "squareSum :: Int -> Int", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)"
 ]

squareSum :: Int -> Int
squareSum 0 = 0
squareSum n = n ^ (2 :: Int) + squareSum (n - 1)

ss4int1 :: Int
ss4int1 = unsafePerformIO $ randomRIO (3, 8)

aboutSquareSum4 :: Page
aboutSquareSum4 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* より詳細な説明をする前に実際に動かしてみよう", \t -> do
	text t "* lectures/lecture06ディレクトリを作成しそこに移動", \t -> do
	text t "* コマンドプロンプトを2こ立ち上げて", \t -> do
	itext t 1 "- エディタでsquareSum.hsを作成しよう", \t -> do
	itext t 2 "squareSum :: Int -> Int", \t -> do
	itext t 2 "squareSum 0 = 0", \t -> do
	itext t 2 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	itext t 1 "- ghci squareSum.hsで読み込もう", \t -> do
	itext t 1 $ "*Main> squareSum " ++ show ss4int1, \t -> do
	itext t 1 $ show $ squareSum ss4int1
 ]

aboutSquareSum5 :: Page
aboutSquareSum5 = [\t -> do
	writeTopTitle t "どうしてこれが動くの?"
	text t "", \t -> do
	text t "* 定義をもう一度見てみよう", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	text t "* 順を追って見ていく", \t -> do
	itext t 1 "n = 1", \t -> do
	itext t 1 "squareSum 1 = 1 ^ 2 + squareSum 0", \t -> do
	itext t 1 "squareSum 1 = 1", \t -> do
	itext t 1 "n = 2", \t -> do
	itext t 1 "squareSum 2 = 2 ^ 2 + squareSum 1", \t -> do
	itext t 1 "squareSum 2 = 4 + 1", \t -> do
	itext t 1 "squareSum 2 = 5"
 ]

aboutSquareSum6 :: Page
aboutSquareSum6 = [\t -> do
	writeTopTitle t "ひとつ前を使う"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "n = 0の値を使ってn = 1の値を求め", \t -> do
	itext t 1 "n = 1の値を使ってn = 2の値を求め", \t -> do
	itext t 1 "n = 2の値を使ってn = 3の値を求め", \t -> do
	itext t 2 "..."
	itext t 1 "n = k - 1の値を使ってn = kの値を求める", \t -> do
	itext t 2 "...", \t -> do
	text t "* よってnが0以上のすべての場合について値が求まる"
 ]

aboutSquareSum7 :: Page
aboutSquareSum7 = [\t -> do
	writeTopTitle t "squareSumの意味"
	text t "", \t -> do
	text t "* もう一度定義を見てみよう", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	text t "* これは以下のように読める", \t -> do
	itext t 1 "0から0の値の二乗の和は0", \t -> do
	itext t 1 "0からnの値の二乗の和は", \t -> do
	itext t 2 "0から(n - 1)の値の二乗の和にn ^ 2を足したもの", \t -> do
	text t "* 動作ではなく「事実」を記述している", \t -> do
	text t "* 動かさなくても字面からプログラムの意味がわかる"
 ]

aboutSquareSum8 :: Page
aboutSquareSum8 = [\t -> do
	writeTopTitle t "再帰関数を作るコツ"
	text t "", \t -> do
	text t "* まずは最もシンプルな部分を作る", \t -> do
	itext t 1 "これを基底と呼ぶ", \t -> do
	text t "* その関数自体がすでに定義されていると考えて", \t -> do
	itext t 1 "より基底に近い定義から次の値を定義する", \t -> do
	text t "* squareSumの場合で考えてみる", \t -> do
	itext t 1 "- 引数0が基底となる", \t -> do
	itext t 1 "- squareSumがすでに定義ずみと考えて", \t -> do
	itext t 2 "squareSum (n -1)を使ってsquareSum nを定義する", \t -> do
	text t "* 重要なのはnより(n - 1)のほうが0に近いこと", \t -> do
	itext t 1 "1を引くという操作をくりかえすと0に到達する"
 ]

aboutSquareSum9 :: Page
aboutSquareSum9 = [\t -> do
	writeTopTitle t "squareSumの評価"
	text t "", \t -> do
	text t "* 「再帰」を理解できればHaskellの半分は理解できたことに", \t -> do
	text t "* 理解のために様々な側面から説明する", \t -> do
	text t "* squareSumの逐次的な評価を見てみよう", \t -> do
	itext t 1 "squareSum 3", \t -> do
	arrowIText t 1 "3 ^ 2 + squareSum 2", \t -> do
	arrowIText t 1 "9 + (2 ^ 2 + squareSum 1)", \t -> do
	arrowIText t 1 "9 + (4 + (1 ^ 2 + squareSum 0))", \t -> do
	arrowIText t 1 "9 + (4 + (1 + 0))", \t -> do
	arrowIText t 1 "14"
 ]

aboutSquareSum10 :: Page
aboutSquareSum10 = [\t -> do
	writeTopTitle t "再帰関数を理解するコツ"
	text t "", \t -> do
	text t "* 再帰関数を見たら2つの方法で理解を試みる", \t -> do
	itext t 1 "- 字面から「何であるか」をとらえようとする", \t -> do
	itext t 1 "- 簡単なケースで逐次的な評価を書きくだす", \t -> do
	text t "* この2つの方法を試すことで仮説を立てることができる", \t -> do
	text t "* 何度かの試行錯誤が必要", \t -> do
	text t "* その関数が何であるか仮説を立てたらそれを検証する"
 ]

aboutSquareSum11 :: Page
aboutSquareSum11 = [\t -> do
	writeTopTitle t "仮説を検証する"
	text t "", \t -> do
	text t "* squareSumの定義を読んで以下の仮説を立てたとする", \t -> do
	itext t 1 "squareSum nは0からnまでの値の二乗の和である", \t -> do
	text t "* 関数の定義がその仮説と合致することを確認する", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	arrowIText t 2 "0のとき仮説は正しい", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	itext t 2 "- 0からnまでの二乗の和 ... (1)", \t -> do
	itext t 2 "- n ^ 2 + 0から(n - 1)までの二乗の和 ... (2)", \t -> do
	itext t 2 "- (1)と(2)は等しい", \t -> do
	text t "* よって仮説と定義は矛盾しない"
 ]

aboutSquareSumSummary  :: Page
aboutSquareSumSummary = [\t -> do
	writeTopTitle t "squareSum(まとめ)"
	text t "", \t -> do
	text t "* squareSumを例に", \t -> do
	itext t 1 "- 再帰関数の定義のしかたを見た", \t -> do
	itext t 1 "- 再帰関数の定義から意味を理解するやりかたを見た", \t -> do
	text t "* 再帰関数を作るときは", \t -> do
	itext t 1 "- 最もシンプルな基底ケースを書く", \t -> do
	itext t 1 "- 「次」の定義を「前」の定義から導く", \t -> do
	text t "* 再帰関数を読むときは", \t -> do
	itext t 1 "- 「字面を見る」や「逐次的評価」によって仮説を立て", \t -> do
	itext t 1 "- 仮説が定義と矛盾しないことを検証する"
 ]

treeRec :: Page
treeRec = [\t -> do
	writeTopTitle t "より複雑な再帰"
	text t "", \t -> do
	text t "* 再帰的定義で「くりかえし」より複雑な制御の流れを定義可", \t -> do
	text t "* 制御の流れが「木構造」となる関数を見ていこう"
 ]
