import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第3回 リスト"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutIterate, aboutIterate2, aboutIterate3, aboutIterate4,
	aboutIterate5,
	iterateSpace
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* プログラミングの機能として「くりかえし」は重要", \t -> do
	text t "* 「くりかえし」を実現するために", \t -> do
	itext t 1 "- 手続き型言語では状態変化を使う", \t -> do
	itext t 1 "- Haskellでは主にリストを使う"
 ]

aboutIterate :: Page
aboutIterate = [\t -> do
	writeTopTitle t "くりかえし"
	text t "", \t -> do
	text t "* 0からnまでの二乗の和を求めるとする", \t -> do
	text t "* 手続き型言語ではこう考える", \t -> do
	itext t 1 "1. iに0を代入、sに0を代入", \t -> do
	itext t 1 "2. iがn以下のあいだ3, 4をくりかえす", \t -> do
	itext t 1 "3. sにiの二乗を足す", \t -> do
	itext t 1 "4. iに1を足す", \t -> do
	itext t 1 "5. sを返す", \t -> do
	text t "* リストを使うと以下のようになる", \t -> do
	itext t 1 "1. 0からnまでの数のリストを作成する", \t -> do
	itext t 1 "2. リストの値のすべてを二乗したリストを作る", \t -> do
	itext t 1 "3. リストの値の総和を求める"
 ]

aboutIterate2 :: Page
aboutIterate2 = [\t -> do
	writeTopTitle t "くりかえし"
	text t "", \t -> do
	text t "* 手続き型のほうは", \t -> do
	itext t 1 "- 本当に正しいのかが自明ではない", \t -> do
	itext t 1 "- 機械の気持ちになって手続きを追うことが必要", \t -> do
	itext t 1 "- iがn以下のとき?それともiがn+1以下のとき?等々", \t -> do
	text t "* リストを使ったやりかたは", \t -> do
	itext t 1 "- 手続きではなく構造を記述している", \t -> do
	itext t 1 "- アルゴリズムが正しいことは自明"
 ]

aboutIterate3 :: Page
aboutIterate3 = [\t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 「リストの値のすべてを二乗したリストを作る」を考える", \t -> do
	text t "* 「リストの値のすべてに何かする」という構造が抽出できる", \t -> do
	text t "* 多くの言語でこのような「構造」は「構文」として特別扱い", \t -> do
	itext t 1 "- C言語のfor文等", \t -> do
	text t "* 引数として関数を取る関数を作れる言語では", \t -> do
	itext t 1 "「構造」を関数として定義することが可能", \t -> do
	text t "* 引数として関数を取る関数は高階関数と呼ばれる", \t -> do
	text t "* 「リストの値のすべてに何かする」関数は用意されている", \t -> do
	itext t 1 "- map f lstでlstのすべての要素にfを適用する"
 ]

square :: Int -> Int
square = (^ (2 :: Int))

ai4number1 :: Int
ai4number1 = unsafePerformIO $ randomRIO (2, 10)

aboutIterate4 :: Page
aboutIterate4 = [\t -> do
	writeTopTitle t "mapを使う"
	text t "", \t -> do
	text t "* 実際にmapを使ってみよう", \t -> do
	text t "* lecture/lecture01ディレクトリに移動して", \t -> do
	text t "* squareSum.hsファイルを作ろう", \t -> do
	text t "* まずはsquare関数を書く", \t -> do
	itext t 1 "square :: Int -> Int"
	itext t 2 "- Intは処理系依存の大きさの整数型", \t -> do
	itext t 1 "square x = x ^ 2", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "% ghci squareSum.hs", \t -> do
	itext t 1 $ "*Main> square " ++ show ai4number1, \t -> do
	itext t 1 $ show $ square ai4number1
 ]

aboutIterate5 :: Page
aboutIterate5 = [\t -> do
	writeTopTitle t "mapを使う"
	text t "", \t -> do
	text t "* 作った関数をghciで確認しながら開発を進めるやりかた", \t -> do
	itext t 1 "- よりシステマティックなquickcheckライブラリもある", \t -> do
	text t "* 関数がだいたい正しいことを確認しながら開発できる", \t -> do
	text t "* square関数を利用して目的の関数を作成する", \t -> do
	itext t 1 "squareAll :: [Int] -> [Int]", \t -> do
	itext t 2 "- 型Xのリストは[X]と書くことができる", \t -> do
	itext t 1 "squareAll ns = map square ns"
 ]

iterateSpace :: Page
iterateSpace = [\t -> do
	writeTopTitle t "空間効率"
	text t "", \t -> do
	text t "* 経験をつんだプログラマならば気になる点がある", \t -> do
	itext t 1 "- いちいちリストを作ると空間効率がひどいことになる", \t -> do
	text t "* それはたしかに正しい", \t -> do
	text t "* しかし、Haskellのリストは遅延リストなので", \t -> do
	itext t 1 "- 必要になるまでリストの要素は作られない", \t -> do
	itext t 1 "- 計算の過程で必要な要素は常にひとつ", \t -> do
	itext t 1 "- また、2度と使われない要素はGCされる", \t -> do
	arrowIText t 1 "空間効率の低下はない", \t -> do
	text t "* 関数のあいだでリストを渡していく形でプログラムを組める", \t -> do
	text t "* 実際には「くりかえし」と同じような動作となる"
 ]

aboutIterateX :: Page
aboutIterateX = [\t -> do
	text t "* 0からnまでの数のリストを作る", \t -> do
	itext t 1 "[0 .. n]"
 ]
