module Main where

import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import Control.Concurrent
import Data.IORef

rt, width, height :: Double
rt = 1
width = 512 * rt
height = 375 * rt

fontName :: String
fontName = "KochiGothic"

bigF, normalF :: Double
bigF = 24 * rt
normalF = 12 * rt

main :: IO ()
main = do
	pagesRef <- newIORef $ zip (map mkSVGFileName [1 .. ]) pages
	f <- openField
--	threadDelay 1000000
	topleft f
	t <- newTurtle f
	shape t "turtle"
	penup t
	titlePage t
	titleSvg <- getSVG t
	writeFile (mkSVGFileName 0) $ showSVG width height titleSvg
	onkeypress f $ \c -> do
		case c of
			'q' -> return False
			' ' -> do
				ps <- readIORef pagesRef
				case ps of
					(fn, p) : ps -> do
						p t
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
title = "Haskell 入門"
author = "重城 良国"

pages :: [Turtle -> IO ()]
pages = [page1, page2]

page1 :: Turtle -> IO ()
page1 t = do
	clear t
	writeTopTitle t "Haskell とは何か?"

page2 :: Turtle -> IO ()
page2 t = do
	text t "今、まさに目の前にある これ"

text :: Turtle -> String -> IO ()
text t txt = do
	setheading t $ - 90
	forward t $ normalF * 2
	setheading t 0
	setx t $ width / 4
	write t fontName normalF txt
	forward t $ normalF * fromIntegral (length txt) * 3 / 2

writeTopTitle :: Turtle -> String -> IO ()
writeTopTitle t ttl = do
	let sz = bigF
	goto t ((width - sz * fromIntegral (length ttl)) / 2) ((height - sz) / 6)
	write t fontName sz ttl
	forward t $ sz * fromIntegral (length ttl)

writeTitle :: Turtle -> String -> IO ()
writeTitle t ttl = do
	let sz = bigF
	goto t ((width - sz * fromIntegral (length ttl)) / 2) ((height - sz) / 2)
	write t fontName sz ttl
	forward t $ sz * fromIntegral (length ttl)

writeRB :: Turtle -> String -> IO ()
writeRB t str = do
	let sz = normalF
--	goto t (width - (sz + 3) * fromIntegral (length str)) (height - sz)
	goto t (width * 3 / 4) (height * 3 / 4)
	write t fontName sz str
	forward t $ width * 3 / 16
