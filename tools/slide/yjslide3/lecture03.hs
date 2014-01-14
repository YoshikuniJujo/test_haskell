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
	coordinate, coordinate2, coordinate3, coordinate4, coordinate5
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 複数の値をまとめた構造をひとつの値として扱える", \t -> do
	text t "* その構造をタプルと呼ぶ", \t -> do
	text t "* タプルを引数として取るときにはパターンマッチが使える", \t -> do
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
	text t "* 型T1, T2, ...を含むタプルの型は以下のようになる", \t -> do
	itext t 1 "(T1, T2, ...)", \t -> do
	text t "* 値x1, x2, ...を含むタプルの値は以下のように書ける", \t -> do
	itext t 1 "(x1, x2, ...)"
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

coordinate5 :: Page
coordinate5 = [\t -> do
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
