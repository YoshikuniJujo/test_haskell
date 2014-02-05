import Control.Monad

import Lecture

subtitle :: String
subtitle = "第22回 まとめ:オセロ(AI)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, testGrid
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* オセロの例を作ってきた", \t -> do
	text t "* 今回はAIの部分を作る", \t -> do
	text t "* マスごとにポイントをふる", \t -> do
	text t "* 何手か先を読み最善手を選ぶ", \t -> do
	text t "* 相手も同じアルゴリズムを採用すると仮定する", \t -> do
	text t "* それなりに強い、と思う", \t -> do
	text t "* 演者よりは強い", \t -> do
	text t "* 演者が対戦すると1勝3敗くらい"
 ]

testGrid :: Page
testGrid = [\t -> do
	writeTopTitle t "test grid"
	text t "", \t -> do
	hideturtle t
	flushoff t
	pensizeRt t 1
	rtGoto t 80 110
	grid t ((show .) . point2) 40 30 8 8
	flushon t
	showturtle t, \t -> do
	rtGoto t 80 110
	gridTriangle t (\x y -> show $ point2 (8 - x - y + 1) (4 - y + 1)) 40 30 4 4
 ]

gridTriangle :: Turtle -> (Int -> Int -> String) -> Double -> Double -> Int -> Int -> IO ()
gridTriangle _ _ _ _ _ 0 = return ()
gridTriangle t s w h nx ny = do
	gridTriangle1 t (flip s ny) w h nx
	setheading t 0
	backwardRt t (fromIntegral (nx - 1) * w)
	setheading t (- 90)
	forwardRt t h
	gridTriangle t (s . (+ 1)) w h (nx - 1) (ny - 1)

gridTriangle1 :: Turtle -> (Int -> String) -> Double -> Double -> Int -> IO ()
gridTriangle1 _ _ _ _ 0 = return ()
gridTriangle1 t s w h n = do
	setheading t 0
	pencolor t "red"
	beginfill t
	replicateM_ 2 $ do
		forwardRt t w
		right t 90
		forwardRt t h
		right t 90
	endfill t
	pencolor t "black"
	forwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	setheading t (- 90)
	forwardRt t (h * 3 / 4)
	writeRt t $ s n
	backwardRt t (h * 3 / 4)
	setheading t 0
	backwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	forwardRt t w
	gridTriangle1 t s w h (n - 1)

point2 :: Int -> Int -> Int
point2 x y
	| x > 4 = point2 (8 - x + 1) y
	| y > 4 = point2 x (8 - y + 1)
	| x < y = point2 y x
point2 1 1 = 120
point2 2 1 = -20
point2 2 2 = -40
point2 3 1 = 20
point2 3 2 = -5
point2 3 3 = 15
point2 4 1 = 5
point2 4 2 = - 5
point2 4 3 = 3
point2 4 4 = 3
point2 x y = error $ "bad square (" ++ show x ++ ", " ++ show y ++ ")"

grid :: Turtle -> (Int -> Int -> String) -> Double -> Double -> Int -> Int -> IO ()
grid _ _ _ _ _ 0 = return ()
grid t s w h nx ny = do
	grid1 t (flip s ny) w h nx
	backwardRt t (w * fromIntegral nx)
	setheading t (- 90)
	forwardRt t h
	grid t s w h nx (ny - 1)

grid1 :: Turtle -> (Int -> String) -> Double -> Double -> Int -> IO ()
grid1 _ _ _ _ 0 = return ()
grid1 t s w h n = do
	setheading t 0
	pendown t
	replicateM_ 2 $ do
		forwardRt t w
		right t 90
		forwardRt t h
		right t 90
	penup t
	forwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	setheading t (- 90)
	forwardRt t (h * 3 / 4)
	writeRt t $ s n
	backwardRt t (h * 3 / 4)
	setheading t 0
	backwardRt t ((w - fromIntegral (length $ s n) * 8) / 2)
	forwardRt t w
	grid1 t s w h (n - 1)
