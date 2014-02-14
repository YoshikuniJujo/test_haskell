module Main (main) where

import Control.Monad
import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第4回 リスト"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutIterate, aboutIterate2, aboutIterate3, aboutIterate4,
	aboutIterate5, enumerate, squareSumFile, squareSumOneL,
	iterateSpace, structure,
	genKatsugi, genKatsugi2, genKatsugi3, genKatsugi4, genKatsugi5,
	addFilter, addFilter2,
	iterateSummary,
	listData, listData2, listData3,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* プログラミングの機能として「くりかえし」は重要", \t -> do
	text t "* 「くりかえし」を実現するために", \t -> do
	itext t 1 "- 手続き型言語では状態変化を使う", \t -> do
	itext t 1 "- Haskellではリストを使うという方法がある"
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
	itext t 1 "1. 0からnまでの数のリストがあり", \t -> do
	itext t 1 "2. そのリストの値のすべてを二乗したリストがあれば", \t -> do
	itext t 1 "3. そのリストの要素の総和が求めるものである"
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
	text t "* 引数として関数をとる関数を作れる言語では", \t -> do
	itext t 1 "「構造」を関数として定義することが可能", \t -> do
	text t "* 引数として関数をとる関数は高階関数と呼ばれる", \t -> do
	text t "* 「リストの値のすべてに何かする」関数は用意されている", \t -> do
	itext t 1 "- map f lstでlstのすべての要素にfを適用する"
 ]

square :: Int -> Int
square = (^ (2 :: Int))

ai4number1 :: Int
ai4number1 = unsafePerformIO $ randomRIO (2, 10)

aboutIterate4 :: Page
aboutIterate4 = [\t -> do
	writeTopTitle t "squareを定義する"
	text t "", \t -> do
	text t "* 実際にmapを使ってみよう", \t -> do
	text t "* lectures/lecture04ディレクトリを作成しそこに移動", \t -> do
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

squareAll :: [Int] -> [Int]
squareAll = map (^ (2 :: Int))

rsa1intList1, _rsa1intList2 :: [Int]
[rsa1intList1, _rsa1intList2] =
	unsafePerformIO $ replicateM 2 $ replicateM 7 $ randomRIO (0, 20)

aboutIterate5 :: Page
aboutIterate5 = [\t -> do
	writeTopTitle t "squareAllを定義する"
	text t "", \t -> do
	text t "* 作った関数をghciで確認しながら開発を進めるやりかた", \t -> do
	text t "* 関数がだいたい正しいことを確認しながら開発できる", \t -> do
	text t "* square関数を利用して目的の関数を作成する", \t -> do
	itext t 1 "squareAll :: [Int] -> [Int]", \t -> do
	itext t 2 "- 型Xのリストは[X]と書くことができる", \t -> do
	itext t 1 "squareAll ns = map square ns", \t -> do
	text t "* 使ってみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> squareAll " ++ show rsa1intList1, \t -> do
	itext t 1 $ show $ squareAll rsa1intList1
 ]

en1int1 :: Int
en1int1 = unsafePerformIO $ randomRIO (4, 9)

squareSum :: Int -> Int
squareSum n = sum $ map (^ (2 :: Int)) [0 .. n]

enumerate :: Page
enumerate = [\t -> do
	writeTopTitle t "0からnまでのリスト"
	text t "", \t -> do
	text t "* 0からnまでのリストを作るのに[x .. y]という構文が使える", \t -> do
	text t "* 0からnまでの数を二乗した数のリストは以下のように表せる", \t -> do
	itext t 1 "squares :: Int -> [Int]", \t -> do
	itext t 1 "squares n = squareAll [0 .. n]", \t -> do
	text t "* 総和を求める関数はすでに用意されているので", \t -> do
	itext t 1 "squareSum :: Int -> Int", \t -> do
	itext t 1 "squareSum n = sum (squares n)", \t -> do
	text t "* 使ってみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> squareSum " ++ show en1int1
	itext t 1 $ show $ squareSum en1int1
 ]

squareSumFile :: Page
squareSumFile = [\t -> do
	writeTopTitle t "できあがったファイル"
	text t "", \t -> do
	text t "square :: Int -> Int"
	text t "square x = x ^ 2"
	text t ""
	text t "squareAll :: [Int] -> [Int]"
	text t "squareAll ns = map square ns"
	text t ""
	text t "squares :: Int -> [Int]"
	text t "squares n = squareAll [0 .. n]"
	text t ""
	text t "squareSum :: Int -> Int"
	text t "squareSum n = sum (squares n)"
 ]

squareSumOneL :: Page
squareSumOneL = [\t -> do
	writeTopTitle t "一行で書ける"
	text t "", \t -> do
	text t "* 関数を積み上げていく開発のやりかたを示した", \t -> do
	text t "* 慣れてくるとはじめから一行で書ける", \t -> do
	itext t 1 "squareSum n = sum $ map (^ 2) [0 .. n]", \t -> do
	text t "* 0からnまでのリストのすべての要素を二乗したものの総和", \t -> do
	itext t 1 "と読める"
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

structure :: Page
structure = [\t -> do
	writeTopTitle t "「くりかえし」の構造"
	text t "", \t -> do
	text t "* 「くりかえし」という動作を3つの部分に分けた", \t -> do
	itext t 1 "- リストを作り出し ([0 .. n])", \t -> do
	itext t 1 "- 要素すべてに関数を適用し (map square)", \t -> do
	itext t 1 "- それらの要素をまとめあげた (sum)", \t -> do
	text t "* それぞれの段階をenumerate, map, accumulateと呼ぶ"
 ]

genKatsugi :: Page
genKatsugi = [\t -> do
	writeTopTitle t "験を担ぐ"
	text t "", \t -> do
	text t "* squareSumを上司のところへ持っていったらこう言われた", \t -> do
	itext t 1 "「バカモノ!4と9は縁起が悪い。二乗の和に含めるな」", \t -> do
	text t "* 0からnまでの4と9以外の数の二乗の和を求めよう", \t -> do
	text t "* squareSum.hsに以下を追加する", \t -> do
	itext t 1 "lucky :: Int -> Bool", \t -> do
	itext t 1 "lucky 4 = False", \t -> do
	itext t 1 "lucky 9 = False", \t -> do
	itext t 1 "lucky _ = True"
 ]

gk2int1 :: Int
gk2int1 = unsafePerformIO $ randomRIO (3, 11)

lucky :: Int -> Bool
lucky 4 = False
lucky 9 = False
lucky _ = True

squareSum' :: Int -> Int
squareSum' n = sum $ map (^ (2 :: Int)) $ filter lucky [0 .. n]

squareSum'' :: Int -> Int
squareSum'' n = sum $ filter lucky $ map (^ (2 :: Int)) $ filter lucky [0 .. n]

genKatsugi2 :: Page
genKatsugi2 = [\t -> do
	writeTopTitle t "験を担ぐ"
	text t "", \t -> do
	text t "* squareSum'を書いてみよう", \t -> do
	itext t 1 "squareSum' :: Int -> Int", \t -> do
	itext t 1 "squareSum' n ="
	itext t 2 "sum $ map (^ 2) $ filter lucky [0 .. n]", \t -> do
	text t "* filterという新しい関数がでてきた", \t -> do
	itext t 1 "filter :: (a -> Bool) -> [a] -> [a]", \t -> do
	text t "* 第一引数で指定した関数でリストの要素をチェックして", \t -> do
	itext t 1 "結果が真のものだけを集めたリストを返す"
 ]

genKatsugi3 :: Page
genKatsugi3 = [\t -> do
	writeTopTitle t "験を担ぐ"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> squareSum " ++ show gk2int1, \t -> do
	itext t 2 "- まずはもともとの関数", \t -> do
	itext t 1 $ show $ squareSum gk2int1, \t -> do
	itext t 1 $ "*Main> squareSum' " ++ show gk2int1, \t -> do
	itext t 1 $ show $ squareSum' gk2int1, \t -> do
	itext t 1 $ "*Main> (squareSum " ++ show gk2int1 ++ ") - (squareSum' " ++
		show gk2int1 ++ ")", \t -> do
	itext t 1 $ show $ squareSum gk2int1 - squareSum' gk2int1, \t -> do
	text t "* ちゃんと4と9の二乗は除かれている"
 ]

genKatsugi4 :: Page
genKatsugi4 = [\t -> do
	writeTopTitle t "験を担ぐ"
	text t "", \t -> do
	text t "* 上司のところに持っていくと", \t -> do
	itext t 1 "「ふーん、いいね」", \t -> do
	itext t 1 "「あ、二乗して4と9になる数も縁起悪いよね」", \t -> do
	itext t 1 "「直しといて」", \t -> do
	text t "* そんな会社やめちまえ", \t -> do
	text t "* と言いたいが、今はまだやめられない理由があるならば", \t -> do
	itext t 1 "squareSum'' :: Int -> Int", \t -> do
	itext t 1 "squareSum'' n = sum $ filter lucky $"
	itext t 2 "map (^ 2) $ filter lucky [0 .. n]"
 ]

gk5int1 :: Int
gk5int1 = unsafePerformIO $ randomRIO (3, 11)

genKatsugi5 :: Page
genKatsugi5 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> squareSum' " ++ show gk5int1, \t -> do
	itext t 1 $ show $ squareSum' gk5int1, \t -> do
	itext t 1 $ "*Main> squareSum'' " ++ show gk5int1, \t -> do
	itext t 1 $ show $ squareSum'' gk5int1, \t -> do
	itext t 1 $ "*Main> (squareSum' " ++ show gk5int1 ++
		") - (squareSum'' " ++ show gk5int1 ++ ")", \t -> do
	itext t 1 $ show $ (squareSum' gk5int1) - (squareSum'' gk5int1), \t -> do
	text t "* 4と9が除かれているのがわかる"
 ]

addFilter :: Page
addFilter = [\t -> do
	writeTopTitle t "「くりかえし」の構造"
	text t "", \t -> do
	text t "* 「くりかえし」という動作を3つの部分に分けた", \t -> do
	itext t 1 "- リストを作り出し", \t -> do
	itext t 1 "- 要素すべてに関数を適用し", \t -> do
	itext t 1 "- それらの要素をまとめあげた", \t -> do
	text t "* それぞれの段階をenumerate, map, accumulateと呼ぶ", \t -> do
	text t "* さらに以下を追加した", \t -> do
	itext t 1 "- 要素のうち条件を満たすものだけを取り出す", \t -> do
	itext t 1 "- これをfilterと呼ぶ"
 ]

addFilter2 :: Page
addFilter2 = [\t -> do
	writeTopTitle t "「くりかえし」の構造"
	text t "", \t -> do
	text t "* 「くりかえし」は以下のように組み立てることができる", \t -> do
	itext t 1 "enumerate", \t -> do
	itext t 1 "複数のmapまたはfilter", \t -> do
	itext t 1 "accumulate"
 ]

iterateSummary :: Page
iterateSummary = [\t -> do
	writeTopTitle t "くりかえし(まとめ)"
	text t "", \t -> do
	text t "* リストを使えば状態変化なしで「くりかえし」が可能", \t -> do
	text t "* 多くの「くりかえし」は以下の構造を持つ", \t -> do
	itext t 1 "- リストを作り(enumerate)", \t -> do
	itext t 1 "- 要素を選び(filter)", \t -> do
	itext t 1 "- それぞれの要素を変換し(map)", \t -> do
	itext t 1 "- それをまとめる(accumulate)", \t -> do
	text t "* リストという実体を渡していくというモデルはわかりやすい", \t -> do
	text t "* 遅延リストとGCによって空間効率は問題なく保たれる"
 ]

listData :: Page
listData = [\t -> do
	writeTopTitle t "データ構造としてのリスト"
	text t "", \t -> do
	text t "* 「くりかえし」の実体化としてのリストを見てきた", \t -> do
	text t "* もちろんリストはデータ構造として使える", \t -> do
	text t "* リストを扱う関数は数多く用意されている", \t -> do
	text t "* また、Haskellの文字列は文字のリストなので", \t -> do
	itext t 1 "それらの関数は文字列に適用することができる", \t -> do
	text t "* 文字列リテラルは構文糖であり以下のようになる", \t -> do
	itext t 1 "\"hello\"", \t -> do
	arrowIText t 1 "['h', 'e', 'l', 'l', o']"
 ]

listData2 :: Page
listData2 = [\t -> do
	writeTopTitle t "データ構造としてのリスト"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> \"hello\"", \t -> do
	itext t 1 $ show "hello", \t -> do
	itext t 1 "*Main> ['h', 'e', 'l', 'l', 'o']", \t -> do
	itext t 1 $ show ['h', 'e', 'l', 'l', 'o'], \t -> do
	text t "* Haskellは文字のリストを文字列として表示してくれる"
 ]

listData3 :: Page
listData3 = [\t -> do
	writeTopTitle t "データ構造としてのリスト"
	text t "", \t -> do
	text t "* リストは本質的には「くりかえし」の実体化である", \t -> do
	text t "* よってデータ構造として使う場合には不得意な分野がある", \t -> do
	text t "* 後ろの要素から順にアクセスするのは不得意", \t -> do
	itext t 1 "- リストは前から作って前から消費するのに向いている", \t -> do
	text t "* 大きなリストを保存しておくのには向かない", \t -> do
	itext t 1 "- 順にGCされていかないような場合には空間効率が悪い", \t -> do
	text t "* よってリストを使うのは以下のいずれか", \t -> do
	itext t 1 "- 本質的に1度だけの「くりかえし」である", \t -> do
	itext t 1 "- 小さなリスト", \t -> do
	itext t 1 "- 効率を考慮しないプロトタイプの作成"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 「くりかえし」の実体化としてのリストについて見た", \t -> do
	text t "* 簡単に使えるデータ構造としてのリストについて見た", \t -> do
	text t "* Haskellでは標準的な文字列は文字のリストである", \t -> do
	text t "* データ構造としてのリストには使いどころがある", \t -> do
	text t "* 大量の要素を保存しランダムアクセスするのには向かない", \t -> do
	text t "* その場合でもプロトタイプであればリストを使うことも"
 ]
