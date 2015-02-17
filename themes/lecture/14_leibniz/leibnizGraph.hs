import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

leibniz :: [Double]
leibniz = map (\n -> (- 1) ** n / (2 * n + 1)) [0 ..]

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	waku t
	penup t
	pencolor t "gray"
	goto t 70 $ 220 - 170 * (pi - 3)
	pendown t
	goto t (90 + 1.8 * 200) $ 220 - 170 * (pi - 3)
	penup t
	goto t (100 + 1.8 * 200) $ 225 - 170 * (pi - 3)
	write t "KochiGothic" 10 "π"
	pencolor t "black"
	let p : ps = take 200 $ map convert prots
	uncurry (goto t) p
	pendown t
	mapM_ (uncurry $ goto t) ps
	hideturtle t
	svg <- getSVG t
	writeFile "leibniz.svg" $ showSVG 500 340 svg
	waitField f

waku :: Turtle -> IO ()
waku t = do
	penup t
	pencolor t "gray"
	goto t 45 $ 220 - 170 * (4.1 - 3)
	pendown t
	goto t 45 $ 220 - 170 * (2.4 - 3)
	yMemori t
	penup t
	goto t (80 + 1.8 * 0) $ 220 - 170 * (2.55 - 3)
	pendown t
	goto t (80 + 1.8 * 200) $ 220 - 170 * (2.55 - 3)
	xMainMemori t
	xMemori t

yMemori :: Turtle -> IO ()
yMemori t = mapM_ (yMemori1 t) [4, 3.5 .. 2.5]

yMemori1 :: Turtle -> Double -> IO ()
yMemori1 t y = do
	penup t
	goto t 45 $ 220 - 170 * (y - 3)
	pendown t
	goto t 50 $ 220 - 170 * (y - 3)
	penup t
	goto t 10 $ 228 - 170 * (y - 3)
	write t "KochiGothic" 15 $ show y

xMainMemori :: Turtle -> IO ()
xMainMemori t = mapM_ (xMainMemori1 t) [0, 100, 200]

xMainMemori1 :: Turtle -> Integer -> IO ()
xMainMemori1 t x = do
	penup t
	goto t (80 + 1.8 * fromIntegral x) $ 220 - 170 * (2.55 - 3)
	pendown t
	goto t (80 + 1.8 * fromIntegral x) $ 210 - 170 * (2.55 - 3)
	penup t
	goto t (80 + 1.8 * fromIntegral x - fromIntegral (length l) * 4) $
		240 - 170 * (2.55 - 3)
	write t "KochiGothic" 15 l
	where
	l = show x

xMemori :: Turtle -> IO ()
xMemori t = do
	mapM_ (xMemori1 t) [10, 20 .. 90]
	mapM_ (xMemori1 t) [110, 120 .. 190]

xMemori1 :: Turtle -> Integer -> IO ()
xMemori1 t x = do
	penup t
	goto t (80 + 1.8 * fromIntegral x) $ 220 - 170 * (2.55 - 3)
	pendown t
	goto t (80 + 1.8 * fromIntegral x) $ 215 - 170 * (2.55 - 3)
	penup t

prots :: [(Int, Double)]
prots = zip [0 .. ] . map (4 *) $ scanl1 (+) leibniz

convert :: (Int, Double) -> (Double, Double)
convert (x, y) = (80 + 1.8 * fromIntegral x, 220 - 170 * (y - 3))
