import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第6回 再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutSquareSum, aboutSquareSum2, aboutSquareSum3, aboutSquareSum4,
	aboutSquareSum5, aboutSquareSum6
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
	itext t 1 "2. 0から(k - 1)までの二乗の和Sがあれば", \t -> do
	itext t 1 "3. 0からkまでの二乗の和はk ^ 2 + S"
 ]

aboutSquareSum3 :: Page
aboutSquareSum3 = [\t -> do
	writeTopTitle t "関数定義"
	text t "", \t -> do
	text t "* 1. 0から0までの二乗の和は0である", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	text t "* 2. 0から(k - 1)までの二乗の和Sがあれば", \t -> do
	itext t 1 "SはsquareSum (k - 1)", \t -> do
	text t "* 3. 0からkまでの二乗の和はk ^ 2 + S", \t -> do
	itext t 1 "squareSum k = k ^ 2 + squareSum (k - 1)", \t -> do
	text t "* よって以下のようになる", \t -> do
	itext t 1 "squareSum :: Int -> Int", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum k = k ^ 2 + squareSum (k - 1)"
 ]

squareSum :: Int -> Int
squareSum 0 = 0
squareSum k = k ^ (2 :: Int) + squareSum (k - 1)

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
	text t "* n = 7のときの値を使ってn = 8"
 ]
