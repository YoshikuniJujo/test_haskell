import Lecture
import Whats
import Control.Monad

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第1回 ミニマルな本質"

pages :: [Page]
pages = [
	titlePage, whats1, whats2, whats3, whats4,
	attention, definition
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

attention :: Page
attention = [\t -> do
	writeTopTitle t "これから見ていく例題について", \t -> do
	text t ""
	semititle t "例題を通してHaskellの本質を見ていきたい", \t -> do
	text t ""
	text t "* わずかな構文でどれだけのことができるかという例", \t -> do
	text t "* Haskellによる抽象化の本質を示す", \t -> do
	text t "* 実際のプログラミングのやりかたを示すわけではない"
 ]

definition :: Page
definition = [\t -> do
	writeTopTitle t "問題定義"
	text t "", \t -> do
	text t "以下のような「街」における"
	text t "地点(x, y) から地点(x', y') への道のりを求める"
	forM_ [0 .. 2] $ \y -> do
		forM_ [0 .. 2] $ \x -> do
			when (y == 0) $
				graphWrite t (indexX x) (indexY y) $ show x
			when (y > 0 && x == 0) $
				graphWrite t (indexX x) (indexY y) $ show y
			drawRect t (blockX x) (blockY y) w h
			when (y == 0 && x == 2) $
				graphWrite t (indexX $ x + 1) (indexY y) $
					show $ x + 1
			when (y == 2 && x == 0) $
				graphWrite t (indexX x) (indexY $ y + 1) $
					show $ y + 1
 ]	where
 	left = 25
	top = 45
	w = 8
	h = 10
	xspace = 3
	yspace = 3
 	indexX x = left + (w + xspace) * fromIntegral x
	indexY y = top + (h + yspace) * fromIntegral y
	blockX x = left + 2.5 + (w + xspace) * fromIntegral x
	blockY y = top + (h + yspace) * fromIntegral y
