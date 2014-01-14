import Control.Applicative
import Control.Monad
import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第3回 タプル"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	coordinate, coordinate2, coordinate3, coordinate4,
	patternMatch, patternMatch2, patternMatch3, patternMatch4,
	patternMatch5, patternMatch6, patternMatch7, patternMatch8,
	patternMatch9,
	currying, currying2, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 複数の値をまとめた構造をひとつの値として扱える", \t -> do
	text t "* その構造をタプルと呼ぶ", \t -> do
	text t "* タプルを引数としてとるときにはパターンマッチが使える", \t -> do
	text t "* タプルとして引数をまとめることができる", \t -> do
	text t "* その逆もでき、それをカリー化と呼ぶ"
 ]

dist0 :: Double -> Double -> Double
dist0 x y = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)

cd1double1, cd1double2 :: Double
[cd1double1, cd1double2] =
	unsafePerformIO $ replicateM 2 $ fromIntegral <$> randomRIO (2 :: Int, 10)

coordinate :: Page
coordinate = [\t -> do
	writeTopTitle t "原点からの距離"
	text t "", \t -> do
	text t "* 直交座標上の点の原点からの距離を求める関数を考える", \t -> do
	text t "* lectures/lecture03を作成しそこに移動", \t -> do
	text t "* coordinate.hsを作成し以下を書き込もう", \t -> do
	itext t 1 "dist0 :: Double -> Double -> Double", \t -> do
	itext t 1 "dist0 x y = sqrt $ x ^ 2 + y ^ 2", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "% ghc coordinate.hs", \t -> do
	itext t 1 $ "*Main> dist0 " ++ show cd1double1 ++ " " ++ show cd1double2
	itext t 1 $ show $ dist0 cd1double1 cd1double2
 ]

px, py :: Double
(px, py) =
	unsafePerformIO $ do
		x <- fromIntegral <$> randomRIO (2 :: Int, 10)
		sg <- getStdGen
		let y = head $ filter (/= x) $ map fromIntegral $
			randomRs (2 :: Int, 10) sg
		return (x, y)

coordinate2 :: Page
coordinate2 = [\t -> do
	writeTopTitle t "点をxとyで表現"
	text t "", \t -> do
	text t "* 点のxとyの値を指定すると原点からの距離を計算する", \t -> do
	text t "* 点pを保存しておいてその点について計算する場合", \t -> do
	itext t 1 "px, py :: Double", \t -> do
	itext t 1 $ "px = " ++ show px, \t -> do
	itext t 1 $ "py = " ++ show py, \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> dist0 px py", \t -> do
	itext t 1 $ show $ dist0 px py
 ]

p :: (Double, Double)
p = (px, py)

coordinate3 :: Page
coordinate3 = [\t -> do
	writeTopTitle t "点を1つの構造として表現"
	text t "", \t -> do
	text t "* 点pをpx, pyという独立したふたつの数ではなく", \t -> do
	itext t 1 "ひとつの構造として表したい", \t -> do
	text t "* このようなときにタプルが使える", \t -> do
	text t "* 点pをタプルを使って表してみよう", \t -> do
	itext t 1 "p :: (Double, Double)", \t -> do
	itext t 1 $ "p = " ++ show p, \t -> do
	text t "* x1 :: T1, x2 :: T2 ...を含むタプルtの定義", \t -> do
	itext t 1 "t :: (T1, T2 ...)", \t -> do
	itext t 1 "t = (x1, x2 ...)"
--	text t "* 型T1, T2, ...を含むタプルの型は以下のようになる", \t -> do
--	itext t 1 "(T1, T2, ...)", \t -> do
--	text t "* 値x1, x2, ...を含むタプルの値は以下のように書ける", \t -> do
--	itext t 1 "(x1, x2, ...)"
 ]

dist0' :: (Double, Double) -> Double
dist0' = uncurry dist0

coordinate4 :: Page
coordinate4 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* coordinate.hsにpの定義を追加しよう", \t -> do
	itext t 1 "p :: (Double, Double)", \t -> do
	itext t 1 $ "p = " ++ show p, \t -> do
	text t "* タプルを使用するdist0'を追加する", \t -> do
	itext t 1 "dist0' :: (Double, Double) -> Double", \t -> do
	itext t 1 "dist0' (x, y) = sqrt $ x ^ 2 + y ^ 2", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> dist0' p", \t -> do
	itext t 1 $ show $ dist0' p
 ]

patternMatch :: Page
patternMatch = [\t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* タプルのそれぞれの要素を使う関数を書く場合", \t -> do
	itext t 1 "パターンマッチを使う", \t -> do
	text t $ "* (x, y)というパターンとpの値" ++ show p ++ "をマッチさせる", \t -> do
	text t "* それぞれ同じ位置にある構造同士をマッチさせるので", \t -> do
	itext t 1 $ "- xを" ++ show px ++ "にマッチさせ", \t -> do
	itext t 1 $ "- yを" ++ show py ++ "にマッチさせる", \t -> do
	text t "* 変数と値をマッチさせるとその変数に値が束縛されるので", \t -> do
	itext t 1 $ "- x, yはそれぞれ" ++ show px ++ ", " ++ show py ++ "となり", \t -> do
	itext t 1 "- それ以降の計算で使うことができる"
 ]

patternMatch2 :: Page
patternMatch2 = [\t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* 点と原点を結ぶ直線の傾きを求める関数を書いてみよう", \t -> do
	text t "* x = 0の場合の扱いにはいくつかの方法がある", \t -> do
	itext t 1 "1. 特殊な値を用意する", \t -> do
	itext t 1 "2. エラーとする", \t -> do
	itext t 1 "3. 適当な値を返す", \t -> do
	text t "* 1が最も良いやりかたで、3が最も悪いやりかた", \t -> do
	text t "* 説明の都合上、今回は3の最も悪いやりかたをする"
 ]

slope :: (Double, Double) -> Double
slope (0, _) = 0
slope (x, y) = y / x

cd7double :: Double
cd7double = unsafePerformIO $ fromIntegral <$> randomRIO (0, 999 :: Int)

patternMatch3 :: Page
patternMatch3 = [\t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* coordinate.hsに以下を追加する", \t -> do
	itext t 1 "slope :: (Double, Double) -> Double", \t -> do
	itext t 1 "slope (0, y) = 0", \t -> do
	itext t 1 "slope (x, y) = y / x", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> slope " ++ show p, \t -> do
	itext t 1 $ show $ slope p, \t -> do
	itext t 1 $ "*Main> slope (0, " ++ show cd7double ++ ")", \t -> do
	itext t 1 $ show $ slope (0, cd7double)
 ]

patternMatch4 :: Page
patternMatch4 = [\t -> do
	writeTopTitle t "パターンマッチ"
	text t "", \t -> do
	text t "* 関数定義が複数ある場合", \t -> do
	itext t 1 "- 上から順にパターンマッチしていき", \t -> do
	itext t 1 "- パターンマッチが成功した定義を評価する", \t -> do
	text t $ "* slope (0, " ++ show cd7double ++ ")の場合", \t -> do
	itext t 1 $ "- (0, " ++ show cd7double ++ ")は(0, y)にマッチし", \t -> do
	itext t 1 $ "- yが" ++ show cd7double ++ "に束縛される", \t -> do
	text t $ "* slope " ++ show p ++ "の場合", \t -> do
	itext t 1 $ "- (0, y)へのマッチは失敗し", \t -> do
	itext t 1 $ "- (x, y)へのマッチが成功し、その定義が使われる"
 ]

patternMatch5 :: Page
patternMatch5 = [\t -> do
	writeTopTitle t "ワイルドカード"
	text t "", \t -> do
	text t "* slope (0, y)の定義のなかでyは使われていない", \t -> do
	text t "* このようなときyの代わりにワイルドカードが使える", \t -> do
	itext t 1 "slope (0, _) = 0", \t -> do
	text t "* ワイルドカードを使う利点", \t -> do
	itext t 1 "1. 使わない変数のための変数名を考えなくてすむ", \t -> do
	itext t 1 "2. その場所の値が使われていないことを明示できる", \t -> do
	text t "* ワイルドカードを使う欠点", \t -> do
	itext t 1 "1. その場所の値が何なのかを示すことができない", \t -> do
	text t "* 欠点1を解消し、利点2を生かすために", \t -> do
	itext t 1 "- 変数名の前に'_'(アンダースコア)をつけて", \t -> do
	itext t 1 "- _yのようにする習慣もある"
 ]

patternMatch6 :: Page
patternMatch6 = [\t -> do
	writeTopTitle t "関数定義"
	text t "", \t -> do
	text t "* 関数定義を以下のように説明した", \t -> do
	itext t 1 "[関数名] [仮引数1] [仮引数2] ... = [表現]", \t -> do
	text t "* パターンマッチを説明した今では以下のようになる", \t -> do
	itext t 1 "[関数名] [パターン1-1] [パターン1-2] ... = [表現1]", \t -> do
	itext t 1 "[関数名] [パターン2-1] [パターン2-2] ... = [表現2]", \t -> do
	itext t 1 "...", \t -> do
	text t "* 変数は「パターン」のひとつであり以下の特徴を持つ", \t -> do
	itext t 1 "- すべての値にマッチし", \t -> do
	itext t 1 "- マッチした結果として、その変数に値が束縛される"
 ]

patternMatch7 :: Page
patternMatch7 = [\t -> do
	writeTopTitle t "パターンとしての「数」"
	text t "", \t -> do
	text t "* slope (0, y)をもう一度見てみる", \t -> do
	text t "* これはネストしたパターンと考えることができる", \t -> do
	text t "* タプルというパターンのなかに", \t -> do
	itext t 1 "- 0というパターンと", \t -> do
	itext t 1 "- yというパターンとがある", \t -> do
	text t "* 数は「パターン」として見ることができ以下の特徴を持つ", \t -> do
	itext t 1 "- 値がその数と同じであるときにマッチする"
 ]

patternMatch8 :: Page
patternMatch8 = [\t -> do
	writeTopTitle t "パターンとしての「数」"
	text t "", \t -> do
	text t "* coordinate.hsを編集していたエディタを閉じて", \t -> do
	text t "* 編集用のコマンドプロンプトでnumber.hsを作成しよう", \t -> do
	itext t 1 "% [エディタ] number.hs", \t -> do
	text t "* 以下を書き込んでみよう('--'で始まる行はコメントなので)", \t -> do
	itext t 1 "intName :: Int -> String", \t -> do
	itext t 2 "-- Stringは文字列を表す型", \t -> do
	itext t 2 "-- 詳細はリストを学んだあとで", \t -> do
	itext t 1 "intName 0 = \"The name is zero.\"", \t -> do
	itext t 1 "intName 1 = \"The name is one.\"", \t -> do
	itext t 1 "intName 2 = \"The name is two.\"", \t -> do
	itext t 1 "intName _ = \"I don't know.\""
 ]

intName :: Int -> String
intName 0 = "The name is zero."
intName 1 = "The name is one."
intName 2 = "The name is two."
intName _ = "I don't know."

pm9int1, pm9int2 :: Int
[pm9int1, pm9int2] = unsafePerformIO $ mapM randomRIO [(0, 2), (2, 10)]

patternMatch9 :: Page
patternMatch9 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	itext t 1 "*Main> :load number.hs", \t -> do
	itext t 1 $ "*Main> intName " ++ show pm9int1, \t -> do
	itext t 1 $ show $ intName pm9int1, \t -> do
	itext t 1 $ "*Main> intName " ++ show pm9int2, \t -> do
	itext t 1 $ show $ intName pm9int2, \t -> do
	text t "* 「数」がパターンとして使えることが確認できた"
 ]

currying :: Page
currying = [\t -> do
	writeTopTitle t "カリー化"
	text t "", \t -> do
	text t "* カリー化とは?", \t -> do
	itext t 1 "複数の引数をとる関数を関数を返す関数に変えること", \t -> do
	text t "* タプルをとる関数を複数の引数をとる関数として考えると", \t -> do
	text t "* dist0'からdist0への変換がカリー化となる", \t -> do
	itext t 1 "dist0'(x, y)", \t -> do
	arrowIText t 1 "(dist0 x) y", \t -> do
	text t "* この逆の変換は非カリー化と呼ばれる", \t -> do
	text t "* 2要素のタプルでそれぞれの変換関数が用意されている", \t -> do
	text t "* 以下の関係が成り立つ", \t -> do
	itext t 1 "dist0 = curry dist0'", \t -> do
	itext t 1 "dist0' = uncurry dist0"
 ]

currying2 :: Page
currying2 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	itext t 1 "*Main> :load coordinate.hs", \t -> do
	itext t 1 $ "*Main> (curry dist0') " ++ show px ++ " " ++ show py, \t -> do
	itext t 1 $ show $ (curry dist0') px py, \t -> do
	itext t 1 $ "*Main> (uncurry dist0) " ++ show p, \t -> do
	itext t 1 $ show $ (uncurry dist0) p, \t -> do
	text t "* curryは以下のように定義される", \t -> do
	itext t 1 "curry :: ((a, b) -> c) -> (a -> b -> c)", \t -> do
	itext t 1 "curry f = \\x y -> f (x, y)", \t -> do
	text t "* uncurryは以下のように定義される", \t -> do
	itext t 1 "uncurry :: (a -> b -> c) -> ((a, b) -> c)", \t -> do
	itext t 1 "uncurry f = \\(x, y) -> f x y"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* タプルについて学んだ", \t -> do
	itext t 1 "t :: (T1, T2 ...)", \t -> do
	itext t 1 "t = (x1, x2 ...)", \t -> do
	text t "* パターンマッチについて学んだ", \t -> do
	itext t 1 "- (p1, p2 ...)というパターンはタプルにマッチする", \t -> do
	itext t 1 "- p1, p2 ...にはそれぞれパターンがはいる", \t -> do
	itext t 1 "- 変数やワイルドカードはすべてにマッチするパターン", \t -> do
	itext t 1 "- 変数の場合マッチと同時に束縛が行われる", \t -> do
	itext t 1 "- 数はそれ自身にマッチするパターン", \t -> do
	text t "* 以下の型の関数は相互に変換可能(カリー化、非カリー化)", \t -> do
	itext t 1 "(a, b) -> c", \t -> do
	itext t 1 "a -> b -> c"
 ]
