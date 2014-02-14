import Control.Monad
import System.Random
import Text.XML.YJSVG
import Graphics.X11.Turtle

ratio :: Double
ratio = 0.5

trial :: Int
trial = 1000

inCircle :: Double -> Double -> Bool
inCircle x y = x ^ (2 :: Int) + y ^ (2 :: Int) <= 10000

randomPoints :: Int -> [(Double, Double)]
randomPoints n = let
	sg = mkStdGen 10
	doubles = randomRs (- 100, 100) sg in
	zip	(map snd $ filter (even . fst) $ zip [0 :: Int ..] doubles)
		(map snd $ filter (odd . fst) $ zip [0 :: Int ..] doubles)

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	penup t
	goto t (- 150 * ratio) (- 100 * ratio)
	pendown t
	replicateM_ 4 $ forward t (200 * ratio) >> left t 90
	penup t
	goto t (- 50 * ratio) (- 100 * ratio)
	pendown t
	circle t (100 * ratio)
	penup t
	speed t "fastest"
	forM_ (take trial $ randomPoints 8) $ \(x, y) -> do
		goto t ((x - 50)  * ratio) (y * ratio)
		if inCircle x y then pencolor t "blue" else pencolor t "red"
		dot t 1
	pencolor t "black"
	goto t (80 * ratio) (- 50 * ratio)
	write t "Monospace" (18 * ratio) $ "in :" ++ show (inCPoints 8 trial)
	goto t (80 * ratio) (- 70 * ratio)
	write t "Monospace" (18 * ratio) $ "out: " ++ show trial
	goto t (80 * ratio) (- 90 * ratio)
	write t "Monospace" (18 * ratio) $ "pi : " ++
		show (fromIntegral (inCPoints 8 trial) / fromIntegral trial * 4)
--	waitField f

	svg <- getSVG t
	putStr $ showSVG (1500 * ratio) (300 * ratio) svg

inCPoints :: Int -> Int -> Int
inCPoints g n = length $ filter (uncurry inCircle) $ take n $ randomPoints g
