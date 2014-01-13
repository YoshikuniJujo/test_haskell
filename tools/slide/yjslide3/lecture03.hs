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
	coordinate, coordinate2
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

cd0double1, cd0double2 :: Double
[cd0double1, cd0double2] =
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
	itext t 1 $ "*Main> dist0 " ++ show cd0double1 ++ " " ++ show cd0double2
	itext t 1 $ show $ dist0 cd0double1 cd0double2
 ]

coordinate2 :: Page
coordinate2 = [\t -> do
	writeTopTitle t "点をxとyで表現"
	text t "", \t -> do
	text t "* 点のxとyの値を指定すると原点からの距離を計算する", \t -> do
	text t "* 点pについて考える"
 ]
