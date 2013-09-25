module Lecture (
	normalF, semiBigF, bigF, biggerF,
	height, width,
	runLecture,
	text, itext, semititle, writeTopTitle, writeNextTitle, writeTitle,
	dvArrow, dvArrowL, dvLArrow, writeImage,
	arrow, rightArrow,
	withRed, drawRect, graphWrite,
) where

import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.Char
import System.Environment
import System.IO.Unsafe

st, sp :: Bool
st = unsafePerformIO $ read <$> readFile "show_turtle.txt"
sp = unsafePerformIO $ read <$> readFile "show_page.txt"

rt, width, height :: Double
rt = unsafePerformIO $ read <$> readFile "ratio.txt"
width = 512 * rt
height = 375 * rt

fontName :: String
fontName = "KochiGothic"

biggerF, bigF, semiBigF, normalF :: Double
biggerF = 36 * rt
bigF = 24 * rt
semiBigF = 15 * rt
normalF = 12 * rt

runLecture :: [Turtle -> IO ()] -> IO ()
runLecture pgs = do
	(bfn, pages', bn) <- (flip fmap getArgs) $ \args -> case args of
		"-" : m : n : _ -> (Nothing,
			take (read n - read m + 1) $
				drop (read m - 1) pgs, read m)
		f : m : n : _ -> (Just f,
			take (read n - read m + 1) $
				drop (read m - 1) pgs, read m)
		"-" : n : _ -> (Nothing, drop (read n - 1) pgs, read n)
		f : n : _ -> (Just f, drop (read n - 1) pgs, read n)
		"-" : _ -> (Nothing, pgs, 1)
		f : _ -> (Just f, pgs, 1)
		_ -> (Nothing, pgs, 1)
	pagesRef <- newIORef $ zip (map (mkSVGFileName bfn) [1 .. ]) pages'
	pageNRef <- newIORef [bn ..]
	let allN = bn + length pages' - 1
	fld <- openField
	topleft fld
	n <- newTurtle fld
	hideturtle n
	penup n
	goto n (width * 44 / 50) (height * 48 / 50)
	t <- newTurtle fld
	shape t "turtle"
	hideturtle t
	penup t
	onkeypress fld $ \c -> do
		case c of
			'q' -> return False
			' ' -> do
				ps <- readIORef pagesRef
				case ps of
					(fn, p) : _ -> do
						when sp $ do
							clear n
							write n fontName (12 * rt)
								. show
								=<< popRef pageNRef
							forward n (24 * rt)
							write n fontName (12 * rt) $ "/" ++ show allN
							backward n (24 * rt)
						when st $ showturtle t
						p t
						sleep t 500
						hideturtle t
						svg <- getSVG t
						case fn of
							Just f -> writeFile f $
								showSVG width
									height svg
							_ -> return ()
						modifyIORef pagesRef tail
						return True
					_ -> return True
			_ -> return True
	waitField fld

popRef :: IORef [a] -> IO a
popRef ref = do
	ret <- head <$> readIORef ref
	modifyIORef ref tail
	return ret

mkSVGFileName :: Maybe String -> Int -> Maybe String
mkSVGFileName (Just bfn) n = Just $ bfn ++ addZero (show n) ++ ".svg"
	where
	addZero s = replicate (2 - length s) '0' ++ s
mkSVGFileName _ _ = Nothing

writeImage :: Turtle -> Double -> Bool -> (Double, Double, FilePath) -> IO ()
writeImage t x bot (w, h, fp) = do
	hideturtle t
	speed t "fastest"
	goto t x (68.5 * rt)
	image t fp (w * rt) (h * rt)
	speed t "slow"
	showturtle t
	when bot $ setheading t (- 90) >> forward t (h * rt)

dvArrowL :: Turtle -> Double -> IO ()
dvArrowL t l = do
	setheading t $ -90
	forward t $ 12 * rt
	pendown t
	forward t $ l * rt
	penup t
	backward t $ l * rt
	left t 90
	forward t $ 6 * rt
	right t 90
	pendown t
	forward t $ l * rt
	setheading t 0
	forward t $ 3 * rt
	beginfill t
	backward t $ 12 * rt
	setheading t $ -60
	forward t $ 12 * rt
	endfill t
	penup t

dvArrow :: Turtle -> IO ()
dvArrow t = do
	setheading t $ -90
	forward t $ 12 * rt
	pendown t
	forward t $ 24 * rt
	penup t
	backward t $ 24 * rt
	left t 90
	forward t $ 6 * rt
	right t 90
	pendown t
	forward t $ 24 * rt
	setheading t 0
	forward t $ 3 * rt
	beginfill t
	backward t $ 12 * rt
	setheading t $ -60
	forward t $ 12 * rt
	endfill t
	penup t

dvLArrow :: Turtle -> Double -> IO ()
dvLArrow t l = do
	setheading t $ -90
	forward t $ 12 * rt
	pendown t
	forward t $ l * rt
	penup t
	backward t $ l * rt
	left t 90
	forward t $ 6 * rt
	right t 90
	pendown t
	forward t $ l * rt
	setheading t 0
	forward t $ 3 * rt
	beginfill t
	backward t $ 12 * rt
	setheading t $ -60
	forward t $ 12 * rt
	endfill t
	penup t

myLength :: String -> Double
myLength "" = 0
myLength (c : cs)
	| isAscii c = 0.7 + myLength cs
	| otherwise = 1.4 + myLength cs

semititle :: Turtle -> String -> IO ()
semititle t txt = do
	setheading t $ - 90
	forward t $ semiBigF * 2
	setheading t 0
	setx t $ width / 12
	write t fontName semiBigF txt
	forward t $ semiBigF * myLength txt

text :: Turtle -> String -> IO ()
text t txt = do
	setheading t $ - 90
	forward t $ normalF * 2
	setheading t 0
	setx t $ width / 8
	write t fontName normalF txt
	forward t $ normalF * myLength txt

itext :: Turtle -> Double -> String -> IO ()
itext t i txt = do
	setheading t $ - 90
	forward t $ normalF * 2
	setheading t 0
	setx t $ width / 8 + i * normalF * 4
	write t fontName normalF txt
	forward t $ normalF * myLength txt

writeTopTitle :: Turtle -> String -> IO ()
writeTopTitle t ttl = do
	let sz = bigF
	hideturtle t
	clear t
	goto t ((width - sz * myLength ttl) / 2) ((height - sz) / 6)
	write t fontName sz ttl
	forward t $ sz * myLength ttl
	setx t $ width / 8
	showturtle t

writeNextTitle :: Turtle -> String -> IO ()
writeNextTitle t ttl = do
	let sz = bigF
	setheading t $ -90
	forward t $ sz * 2
	left t 90
	setx t $ (width - sz * myLength ttl) / 2
	write t fontName sz ttl
	forward t $ sz * myLength ttl

writeTitle :: Turtle -> String -> String -> String -> IO ()
writeTitle t ttl subTtl at = do
	let	sz = biggerF
		szn = normalF
	hideturtle t
	speed t "fastest"
	goto t ((width - sz * myLength ttl) / 2) ((height - sz) / 2)
	write t fontName sz ttl
	forward t $ sz * myLength ttl
	goto t ((width - szn * myLength subTtl) / 2) ((height - sz) / 2 + szn * 2)
	write t fontName szn subTtl
	forward t $ szn * myLength subTtl
	writeRB t at
	speed t "slow"

writeRB :: Turtle -> String -> IO ()
writeRB t str = do
	let sz = normalF
	goto t (width * 3 / 4) (height * 7 / 8)
	write t fontName sz str
	forward t $ width * 3 / 16

withRed :: Turtle -> IO a -> IO a
withRed t act = do
	pencolor t "red"
	r <- act
	pencolor t "black"
	return r

arrow :: Turtle -> Double -> IO ()
arrow t l = do
	pendown t
	pensize t $ 3 * rt
	forward t l
	left t 90
	penup t
	backward t (6 * rt)
	beginfill t
	forward t (12 * rt)
	right t 120
	forward t (12 * rt)
	endfill t
	pensize t $ 1 * rt

rightArrow :: Turtle -> IO ()
rightArrow t = do
	setheading t $ -90
	forward t $ normalF * 55 / 32
	left t 90
	arrow t $ 12 * rt
	left t 90
	forward t $ normalF * 55 / 32

drawRect :: Turtle -> Double -> Double -> IO ()
drawRect t w h = do
	pensize t $ 2 * rt
	pendown t
	replicateM_ 2 $ forward t w >> right t 90 >> forward t h >> right t 90
	penup t
	pensize t $ 1 * rt

graphWrite :: Turtle -> String -> IO ()
graphWrite t str = write t fontName semiBigF str
