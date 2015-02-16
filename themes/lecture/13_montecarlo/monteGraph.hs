import Control.Arrow
import Control.Monad
import System.Random
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

points :: Int -> [(Double, Double)]
points = uncurry zip . (randomRs (-1, 1) *** randomRs (-1, 1)) . split . mkStdGen

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x ^ 2 + y ^ 2 <= 1

inCirclePoints :: Int -> Int -> [(Double, Double)]
inCirclePoints g n = filter inCircle . take n $ points g

guessPi :: Int -> Int -> Double
guessPi g n = 4 * fromIntegral (length $ inCirclePoints g n) / fromIntegral n

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	drawGraph t [
		(8, "black"),
		(9, "red"),
		(10, "green"),
		(11, "blue"),
		(12, "brown") ]
	svg <- getSVG t
	writeFile "monteGraph.svg" $ showSVG 600 400 svg
	waitField f

drawGraph :: Turtle -> [(Int, String)] -> IO ()
drawGraph t ts = do
	waku t
	lavels t ts
	penup t
	mapM_ (uncurry $ drawTrial t) ts
	hideturtle t

drawTrial :: Turtle -> Int -> String -> IO ()
drawTrial t g c = do
	let poss = map convert $ ps g
	pencolor t c
	uncurry (goto t) $ head poss
	pendown t
	mapM_ (uncurry $ goto t) $ tail poss
	penup t

ps :: Int -> [(Int, Double)]
ps g = map (\n -> (n, guessPi g n)) [1000, 2000 .. 100000]

convert :: (Int, Double) -> (Double, Double)
convert (n, p) = (80 + fromIntegral n / 290, 450 - 1600 * (p - 3))

waku :: Turtle -> IO ()
waku t = do
	pencolor t "gray"
	penup t
	goto t 30 $ 450 - 1600 * (3.08 - 3)
	pendown t
	goto t 30 $ 450 - 1600 * (3.27 - 3)
	penup t
	goto t 30 $ 450 - 1600 * (3.1 - 3)
	pendown t
	goto t 35 $ 450 - 1600 * (3.1 - 3)
	penup t
	goto t 10 $ 455 - 1600 * (3.1 - 3)
	write t "KochiGothic" 10 "3.1"
	goto t 30 $ 450 - 1600 * (3.2 - 3)
	pendown t
	goto t 35 $ 450 - 1600 * (3.2 - 3)
	penup t
	goto t 10 $ 455 - 1600 * (3.2 - 3)
	write t "KochiGothic" 10 "3.2"
	goto t 40 $ 450 - 1600 * (pi - 3)
	pendown t
	goto t 475 $ 450 - 1600 * (pi - 3)
	penup t
	goto t 480 $ 455 - 1600 * (pi - 3)
	write t "KochiGothic" 10 "π"
	memori t

memori :: Turtle -> IO ()
memori t = do
	goto t (80 + 0 / 290) $ 490 - 1600 * (3.1 - 3)
	pendown t
	goto t (80 + 100000 / 290) $ 490 - 1600 * (3.1 - 3)
	penup t
	mapM_ (putMemori t . (* 10000)) [0 .. 10]
	penup t
	goto t (78 + 0 / 290) $ 505 - 1600 * (3.1 - 3)
	write t "KochiGothic" 10 "0"
	goto t (78 + 100000 / 290) $ 505 - 1600 * (3.1 - 3)
	write t "KochiGothic" 10 "100000"

putMemori :: Turtle -> Int -> IO ()
putMemori t n = do
	penup t
	goto t (80 + fromIntegral n / 290) $ 490 - 1600 * (3.1 - 3)
	pendown t
	goto t (80 + fromIntegral n / 290) $ 485 - 1600 * (3.1 - 3)

lavels :: Turtle -> [(Int, String)] -> IO ()
lavels t ts = do
	penup t
	zipWithM_ (lavel t) [0 ..] ts

lavel :: Turtle -> Int -> (Int, String) -> IO ()
lavel t n (g, c) = do
	goto t 350 (50 + 16 * fromIntegral n)
	pendown t
	pensize t 2
	pencolor t c
	goto t 365 (50 + 16 * fromIntegral n)
	pensize t 1
	penup t
	pencolor t "black"
	goto t 370 (55 + 16 * fromIntegral n)
	write t "KochiGothic" 10 $ "g = " ++ show g
