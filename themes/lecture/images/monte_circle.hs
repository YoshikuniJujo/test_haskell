import Control.Applicative
import Control.Monad
import System.Random
import System.Environment
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

-- dotsNum :: Int
-- dotsNum = 10000

main :: IO ()
main = do
	n : fn : _ <- getArgs
	let dotsNum = read n
	f <- openField
	onkeypress f (return . (/= 'q'))
	topleft f
	t <- newTurtle f
	penup t
	goto t 10 10
	pendown t
	replicateM 4 $ forward t 200 >> right t 90
	forward t (100 - pi * 10 / 36)
	replicateM 360 $ forward t (pi * 20 / 36) >> right t 1
	penup t
	hideturtle t
--	dots <- map checkDot . take dotsNum <$> randomDots
	let	dots = map checkDot . take dotsNum $ randomDotsG 8
		ind = length $ filter snd dots
	mapM (putDot t) dots
	pencolor t "black"
	goto t 220 30
	write t "KochiGothic" 15 $ "全ての点: " ++ show dotsNum
	goto t 220 50
	write t "KochiGothic" 15 $ "円内の点: " ++ show ind
	goto t 220 70
	write t "KochiGothic" 15 $ "円周率: " ++ show
		(fromIntegral ind * 4 / fromIntegral dotsNum)
	svg <- getSVG t
--	writeFile "monte_circle.svg" $ showSVG 350 220 svg
	writeFile fn $ showSVG 350 220 svg
	waitField f

randomDotsG :: Int -> [(Double, Double)]
randomDotsG n = let
	g0 = mkStdGen n
	(gx, gy) = split g0 in
	zip (randomRs (-1, 1) gx) (randomRs (-1, 1) gy)

randomDots :: IO [(Double, Double)]
randomDots = do
	gx <- newStdGen
	gy <- newStdGen
	return $ zip (randomRs (-1, 1) gx) (randomRs (-1, 1) gy)

checkDot :: (Double, Double) -> ((Double, Double), Bool)
checkDot p@(x, y) = (p, x ^ 2 + y ^ 2 <= 1)

selColor :: Turtle -> Bool -> IO ()
selColor t c = pencolor t $ if c then "red" else "blue"

putDot :: Turtle -> ((Double, Double), Bool) -> IO ()
putDot t ((x, y), c) = do
	goto t (110 + x * 100) (110 + y * 100)
	selColor t c
	dot t 1
