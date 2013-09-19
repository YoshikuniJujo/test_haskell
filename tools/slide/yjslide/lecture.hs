module Main where

import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Char
import System.Environment

rt, width, height :: Double
rt = 1
width = 512 * rt
height = 375 * rt

fontName :: String
fontName = "KochiGothic"

bigF, semiBigF, normalF :: Double
bigF = 24 * rt
semiBigF = 15 * rt
normalF = 12 * rt

main :: IO ()
main = do
	pages' <- (flip fmap getArgs) $ \args -> case args of
		n : _ -> drop (read n) pages
		_ -> pages
	pagesRef <- newIORef $ zip (map mkSVGFileName [0 .. ]) pages'
	f <- openField
--	threadDelay 1000000
	topleft f
	t <- newTurtle f
	shape t "turtle"
	penup t
	onkeypress f $ \c -> do
		case c of
			'q' -> return False
			' ' -> do
				ps <- readIORef pagesRef
				case ps of
					(fn, p) : ps -> do
						showturtle t
						p t
						sleep t 500
						hideturtle t
						svg <- getSVG t
						writeFile fn $
							showSVG width height svg
						modifyIORef pagesRef tail
						return True
					_ -> return False
			_ -> return True
	waitField f

mkSVGFileName :: Int -> String
mkSVGFileName n = "lecture" ++ addZero (show n) ++ ".svg"
	where
	addZero s = replicate (2 - length s) '0' ++ s

titlePage :: Turtle -> IO ()
titlePage t = do
	writeTitle t title
	writeRB t author

title, author :: String
title = "Haskell入門"
author = "重城 良国"

pages :: [Turtle -> IO ()]
pages = [titlePage, page1, page2, page3, page4, page5, page6]

page1 :: Turtle -> IO ()
page1 t = do
	clear t
	writeTopTitle t "Haskellとは何か?"

page2 :: Turtle -> IO ()
page2 t = do
	setheading t $ - 90
	forward t $ 10 * rt
	setx t $ width / 3
	image t "HaskellBCurry.jpg" (279 * rt / 2) (343 * rt / 2)
	forward t (343 * rt / 2)
	text t "Haskell Brooks Curry (1900.9.12 - 1982 9.1)"
	text t "アメリカの記号論理学者"
	text t "名前の由来はこの人"

page3 :: Turtle -> IO ()
page3 t = do
	replicateM_ 23 $ undo t
	setx t $ width * 2 / 3
	image t "HaskellBCurry.jpg" (279 * rt / 2) (343 * rt / 2)
	semititle t "純粋関数型言語"
	text t "* 第一級関数"
	text t "* 参照透過"
	text t "* 静的型付"
	text t "* 遅延評価"

page4 :: Turtle -> IO ()
page4 t = do
	backward t $ 50 * rt
	dvArrow t
	text t "概念の本質的な部分をそのまま表現できる"

page5 :: Turtle -> IO ()
page5 t = do
	clear t
	writeTopTitle t "関数とは?"

page6 :: Turtle -> IO ()
page6 t = do
	text t ""
	text t "入力値を出力値へ変えるルール"
	goto t (width * 1 / 10) (height * 5 / 10)
	mkFunGraph t

mkFunGraph :: Turtle -> IO ()
mkFunGraph t = do
	write t fontName semiBigF "入力1"
	setheading t $ - 90
	forward t (height * 1 / 5)
	write t fontName semiBigF "入力2"
	backward t (height * 1 / 5 + semiBigF / 2)
	left t 90
	forward t (width * 3 / 20)
	x <- xcor t
	arrow t (width * 1 / 10)
	setheading t $ - 90
	forward t (height * 1 / 5)
	left t 90
	setx t x
	arrow t (width * 1 / 10)
--	goto t (

arrow :: Turtle -> Double -> IO ()
arrow t l = do
	pendown t
	pensize t (3 * rt)
	forward t l
	left t 90
	penup t
	backward t (6 * rt)
	beginfill t
	forward t (12 * rt)
	right t 120
	forward t (12 * rt)
	endfill t

{-
page6 :: Turtle -> IO ()
page6 t = do
	clear t
	writeTopTitle t "第一級関数とは?"

page7 :: Turtle -> IO ()
page7 t = do
	text t "関数が第一級オブジェクトであるということ"
	-}

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
	forward t 3
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
	setx t $ width / 6
	write t fontName normalF txt
	forward t $ normalF * myLength txt

writeTopTitle :: Turtle -> String -> IO ()
writeTopTitle t ttl = do
	let sz = bigF
	goto t ((width - sz * myLength ttl) / 2) ((height - sz) / 6)
	write t fontName sz ttl
	forward t $ sz * myLength ttl

writeTitle :: Turtle -> String -> IO ()
writeTitle t ttl = do
	let sz = bigF
	goto t ((width - sz * myLength ttl) / 2) ((height - sz) / 2)
	write t fontName sz ttl
	forward t $ sz * myLength ttl

writeRB :: Turtle -> String -> IO ()
writeRB t str = do
	let sz = normalF
	goto t (width * 3 / 4) (height * 3 / 4)
	write t fontName sz str
	forward t $ width * 3 / 16
