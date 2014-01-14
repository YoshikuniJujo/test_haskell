module Lecture (
	Page,
	runLecture,
	text, itext, semititle, isemititle,
	writeTopTitle, writeNextTitle, writeTitle, writeTitleTT,
	dvArrow, dvArrowShort, writeImageCenter, writeImageRight,
	arrow, rightArrow, withRed, drawRect, graphWrite,
	graphArrowString, xmark, arrowIText, preLine,
	haskellBCurry, oneshot,
	itext', hLine,
	Turtle,
	writeTree, BinTree(..), rtGoto, rotateL, rotateR, width, mapR,
	randomDot, drawRect2, forwardRt, circleRt, dotRt,
	newTurtle, field, hideturtle, penup, writeRt, undo,
) where

import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.Char
import Data.List
import System.Environment
import System.IO.Unsafe
import System.Random

import qualified WriteTree as WT
import WriteTree(BinTree(..), rotateL, rotateR, mapR)

type Page = [Turtle -> IO ()]

title, author :: String
title = "Haskell入門"
author = "重城 良国"

haskellBCurry :: (Double, Double, FilePath)
haskellBCurry = (139.5, 171.5, "HaskellBCurry.jpg")

st, sp :: Bool
st = unsafePerformIO $ read <$> readFile "show_turtle.txt"
sp = unsafePerformIO $ read <$> readFile "show_page.txt"

rt, width, height :: Double
rt = unsafePerformIO $ read <$> readFile "ratio.txt"
width = 512 * rt
height = 375 * rt

rtGoto :: Turtle -> Double -> Double -> IO ()
rtGoto t x y = goto t (x * rt) (y * rt)

fontName :: String
fontName = "KochiGothic"

biggerF, bigF, semiBigF, normalF :: Double
biggerF = 36 * rt
bigF = 24 * rt
semiBigF = 15 * rt
normalF = 12 * rt

runLecture :: [[Turtle -> IO ()]] -> IO ()
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
	let pages'' = intercalate [const $ return ()] pages'
	pagesRef <- newIORef $ zip (map (mkSVGFileName bfn) [1 .. ]) pages''
	pageNRef <- newIORef $ concat $
		zipWith replicate (map ((+ 1) . length) pages') [bn ..]
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
	addZero s = replicate (3 - length s) '0' ++ s
mkSVGFileName _ _ = Nothing

writeImageRight :: Turtle -> (Double, Double, FilePath) -> IO ()
writeImageRight t picture = writeImage t (width * 2 / 3) 68.5 False picture

writeImageCenter :: Turtle -> Double -> (Double, Double, FilePath) -> IO ()
writeImageCenter t y picture@(w, _, _) =
	writeImage t ((width - w * rt) / 2) y True picture

writeImage ::
	Turtle -> Double -> Double -> Bool -> (Double, Double, FilePath) -> IO ()
writeImage t x y bot (w, h, fp) = do
--	hideturtle t
--	speed t "fastest"
	goto t x (y * rt)
	image t fp (w * rt) (h * rt)
--	speed t "slow"
--	showturtle t
	when bot $ setheading t (- 90) >> forward t (h * rt)

dvArrowL :: Turtle -> Double -> IO ()
dvArrowL t l = do
	setx t $ width / 3
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

dvArrowShort :: Turtle -> IO ()
dvArrowShort t = dvArrowL t 12

dvArrow :: Turtle -> IO ()
dvArrow t = do
	setx t $ width / 3
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

isemititle :: Turtle -> Double -> String -> IO ()
isemititle t i txt = do
	setheading t $ - 90
	forward t $ normalF * 2
	setheading t 0
	setx t $ width / 12 + i * semiBigF * 4
	write t fontName semiBigF txt
	forward t $ semiBigF * myLength txt

writeRt :: Turtle -> String -> IO ()
writeRt t txt = write t fontName normalF txt

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

itext' :: Turtle -> Double -> String -> IO ()
itext' t i txt = do
	setheading t $ - 90
	forward t $ normalF * 2
	setheading t 0
	setx t $ width / 8 + i * normalF * 4
	write' t fontName normalF txt

write' :: Turtle -> String -> Double -> String -> IO ()
write' _ _ _ "" = return ()
write' t fn fs ('_' : '{' : ca)
	= do
		let	(c, '}' : cs) = span (/= '}') ca
		write t fn (fs * 3 / 4) c
		forward t (normalF * 1 / 2 * fromIntegral (length c))
		write' t fn fs cs
write' t fn fs (c : cs)
	= do
		write t fn fs [c]
		forward t (normalF * 2 / 3)
		write' t fn fs cs

writeTopTitle :: Turtle -> String -> IO ()
writeTopTitle t ttl = do
	let sz = bigF
	hideturtle t
	setheading t 0
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

writeTitle :: Turtle -> String -> IO ()
writeTitle = writeTitleGen title

writeTitleTT :: Turtle -> String -> IO ()
writeTitleTT = writeTitleGen "座談会"

writeTitleGen :: String -> Turtle -> String -> IO ()
writeTitleGen tt t subTtl = do
	let	sz = biggerF
		szn = normalF
	hideturtle t
	speed t "fastest"
	goto t ((width - sz * myLength tt) / 2) ((height - sz) / 2)
	write t fontName sz tt
	forward t $ sz * myLength title
	goto t ((width - szn * myLength subTtl) / 2) ((height - sz) / 2 + szn * 2)
	write t fontName szn subTtl
	forward t $ szn * myLength subTtl
	writeRB t author
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
	backward t (5 * rt)
	beginfill t
	forward t (10 * rt)
	right t 120
	forward t (10 * rt)
	endfill t
	pensize t $ 1 * rt

rightArrow :: Turtle -> IO ()
rightArrow t = do
	setheading t $ -90
	forward t $ normalF * 3 / 2
	left t 90
	arrow t $ 8 * rt
	left t 90
	forward t $ normalF * 55 / 32

drawRect :: Turtle -> Double -> Double -> Double -> Double -> IO ()
drawRect t x y w h = do
	setheading t 0
	goto t (width * x / 100) (height * y / 100)
	pensize t $ 2 * rt
	pendown t
	replicateM_ 2 $
		forward t (width * w / 100) >> right t 90 >>
		forward t (height * h / 100) >> right t 90
	penup t
	pensize t $ 1 * rt

drawRect2 :: Turtle -> Double -> Double -> Double -> Double -> IO ()
drawRect2 t x y w h = do
	setheading t 0
	goto t (x * rt) (y * rt)
	pensize t $ 2 * rt
	pendown t
	replicateM_ 2 $
		forward t (rt * w) >> right t 90 >>
		forward t (rt * h) >> right t 90
	penup t
	pensize t $ 1 * rt

forwardRt :: Turtle -> Double -> IO ()
forwardRt t = forward t . (* rt)

circleRt :: Turtle -> Double -> IO ()
circleRt t r = do
	pendown t
	setheading t 180
	circle t (r * rt)
	penup t

graphWrite :: Turtle -> Double -> Double -> String -> IO ()
graphWrite t x y str = do
	goto t (width * x / 100) (height * y / 100)
	write t fontName semiBigF str

graphArrowString ::
	Turtle -> Double -> Double -> Maybe String -> Maybe String -> IO ()
graphArrowString t x y bstr astr = do
	goto t (width * x / 100) (height * y / 100)
	setheading t 0
	flip (maybe $ return ()) bstr $ \s -> do
		write t fontName semiBigF s
		forward t $ semiBigF * (myLength s + 1)
	left t 90
	forward t $ semiBigF / 2
	right t 90
	arrow t (width / 10)
	forward t $ semiBigF
	right t 90
	maybe (return ()) (write t fontName semiBigF) astr

xmark :: Turtle -> String -> IO ()
xmark t str = do
	let v = normalF * 9 / 8
	setheading t 90
	forward t v
	setx t $ width / 8
	y <- ycor t
	pensize t 2
	pendown t
	goto t (width / 8 + myLength str * normalF) (y + v)
	penup t
	forward t v
	pendown t
	goto t (width / 8) (y + v)
	penup t
	pensize t 1

arrowIText :: Turtle -> Double -> String -> IO ()
arrowIText t i str = do
	setx t $ width * 5 / 64 + 4 * i * normalF
	rightArrow t
	itext t i str

preLine :: Turtle -> IO ()
preLine t = setheading t 90 >> forward t (normalF * 2)

oneshot :: Turtle -> IO () -> IO ()
oneshot t act = do
	hideturtle t
	act
	showturtle t

hLine :: Turtle -> Double -> Double -> IO ()
hLine t b l = do
	setheading t $ - 90
	forward t $ normalF / 2
	left t 90
	setx t $ b * width / 100
	pendown t
	forward t $ l * width / 100
	penup t

writeTree :: Turtle -> (a -> String) -> Double -> Double -> Double -> Double -> BinTree a -> IO ()
writeTree t sw sz dx x y = WT.writeTree t sw (sz * rt) dx (x * rt) (y * rt)

randomDot :: Turtle -> Double -> Double -> Double -> Double -> IO ()
randomDot t lft tp w h = do
	x <- randomRIO (lft * rt, lft * rt + w * rt)
	y <- randomRIO (tp * rt, tp * rt + h * rt)
	goto t x y
	dot t (1 * rt)

dotRt :: Turtle -> Double -> Double -> IO ()
dotRt t x y = do
	goto t x y
	dot t (1 * rt)
