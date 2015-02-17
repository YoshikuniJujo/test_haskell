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
	writeFile "monteGraphLog.svg" $ showSVG 500 400 svg
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
ps g = map (\n -> (n, guessPi g $ 2 ^ n)) [0 .. 20]

convert :: (Int, Double) -> (Double, Double)
convert (n, p) = (80 + fromIntegral n * 18, 130 - 100 * (p - 3))

waku :: Turtle -> IO ()
waku t = do
	pencolor t "gray"
	penup t
	goto t 45 $ 130 - 100 * (1 - 3)
	pendown t
	goto t 45 $ 130 - 100 * (4 - 3)
	penup t
	yMemori t 3.5
	yMemori t 3
	yMemori t 2.5
	yMemori t 2
	yMemori t 1.5
	goto t 50 $ 130 - 100 * (pi - 3)
	pendown t
	goto t 475 $ 130 - 100 * (pi - 3)
	penup t
	goto t 480 $ 135 - 100 * (pi - 3)
	write t "KochiGothic" 15 "π"
	memori t

yMemori :: Turtle -> Double -> IO ()
yMemori t y = do
	penup t
	goto t 45 $ 130 - 100 * (y - 3)
	pendown t
	goto t 50 $ 130 - 100 * (y - 3)
	penup t
	goto t 10 $ 138 - 100 * (y - 3)
	write t "KochiGothic" 15 $ show y

-- convert (n, p) = (80 + fromIntegral n * 18, 130 - 100 * (p - 3))

memori :: Turtle -> IO ()
memori t = do
	goto t (80 + 0 * 18) $ 130 - 100 * (1 - 3)
	pendown t
	goto t (80 + 20 * 18) $ 130 - 100 * (1 - 3)
	penup t
	mapM_ (putMemori t) [1 .. 9]
	mapM_ (putMemori t) [11 .. 19]
	mapM_ (putMainMemori t) [0, 10 .. 20]

putMainMemori :: Turtle -> Int -> IO ()
putMainMemori t n = do
	penup t
	goto t (80 + fromIntegral n * 18) $ 130 - 100 * (1 - 3)
	pendown t
	goto t (80 + fromIntegral n * 18) $ 120 - 100 * (1 - 3)
	penup t
	goto t (80 + fromIntegral n * 18 - fromIntegral (length l) * 4)
		$ 150 - 100 * (1 - 3)
	write t "KochiGothic" 15 l
	where
	l = show $ 2 ^ n

putMemori :: Turtle -> Int -> IO ()
putMemori t n = do
	penup t
	goto t (80 + fromIntegral n * 18) $ 130 - 100 * (1 - 3)
	pendown t
	goto t (80 + fromIntegral n * 18) $ 125 - 100 * (1 - 3)

lavels :: Turtle -> [(Int, String)] -> IO ()
lavels t ts = do
	penup t
	zipWithM_ (lavel t) [0 ..] ts

lavel :: Turtle -> Int -> (Int, String) -> IO ()
lavel t n (g, c) = do
	goto t 350 (175 + 22 * fromIntegral n)
	pendown t
	pensize t 3
	pencolor t c
	goto t 365 (175 + 22 * fromIntegral n)
	pensize t 1
	penup t
	pencolor t "black"
	goto t 375 (182 + 22 * fromIntegral n)
	write t "KochiGothic" 15 $ "g = " ++ show g
