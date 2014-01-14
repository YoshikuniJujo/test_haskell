import Control.Monad
import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第5回 演習(1日目)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	montecarlo, getPiAlgorithm
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今日は関数、型、タプル、リストについて学んだ", \t -> do
	text t "* タプルのところではパターンマッチについても学んだ", \t -> do
	text t "* 今の知識でできる演習問題を見ていくことにしよう", \t -> do
	text t "* モンテカルロ法で円周率を求める方法を見ていこう", \t -> do
	text t "* 必要となる関数を演習問題として出していこう"
 ]

montecarlo :: Page
montecarlo = [\t -> do
	writeTopTitle t "モンテカルロ法"
	text t "", \t -> do
	text t "* モンテカルロとは?", \t -> do
	itext t 1 "- カジノで有名なモナコ公国の1地区", \t -> do
	text t "* モンテカルロ法とは?", \t -> do
	itext t 1 "- 乱数を使うことで「決まった時間内」に", \t -> do
	itext t 1 "- 「正しい可能性の高い結果」が得られる", \t -> do
	text t "* 本当に正しいかどうかはわからない", \t -> do
	itext t 1 "- 一様の賭けである", \t -> do
	itext t 1 "- だから「モンテカルロ」法"
 ]

randomXY :: [(Double, Double)]
randomXY = unsafePerformIO $ replicateM 300 $ do
	x <- randomRIO (-1, 1)
	y <- randomRIO (-1, 1)
	return (x, y)

getPiAlgorithm :: Page
getPiAlgorithm = [\t -> do
	writeTopTitle t "アルゴリズム"
	text t "", \t -> do
	text t "* 円周率は半径1の円の面積に等しい", \t -> do
	text t "* 1辺が2の正方形のなかに半径1の円を書く", \t -> do
	text t "* ランダムに点を打つ", \t -> do
	text t "* 以下の値が円の面積に近づいていくと予測できる", \t -> do
	itext t 1"円の中にある点の数 / 全体の点の数 * 4", \t -> do
	drawRect2 t 150 220 120 120
	forwardRt t 60
	circleRt t 60
	t' <- newTurtle (field t)
	hideturtle t'
	penup t'
	rtGoto t' 300 300
	writeRt t' $ show ((0, 0) :: (Int, Int))
--	replicateM_ 300 $ randomDot t 150 220 120 120
	forM_ randomXY $ \(x, y) -> do
		dotRt t (210 + 60 * x) (280 + 60 * y)
		undo t'
		writeRt t' $ show (x, y)
 ]
