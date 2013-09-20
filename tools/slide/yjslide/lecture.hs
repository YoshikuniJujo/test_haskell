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
	(bfn, pages') <- (flip fmap getArgs) $ \args -> case args of
		"-" : m : n : _ -> (Nothing,
			take (read n - read m) $ drop (read m) pages)
		f : m : n : _ -> (Just f,
			take (read n - read m) $ drop (read m) pages)
		"-" : n : _ -> (Nothing, drop (read n) pages)
		f : n : _ -> (Just f, drop (read n) pages)
		"-" : _ -> (Nothing, pages)
		f : _ -> (Just f, pages)
		_ -> (Nothing, pages)
	pagesRef <- newIORef $ zip (map (mkSVGFileName bfn) [0 .. ]) pages'
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
						case fn of
							Just f -> writeFile f $
								showSVG width
									height svg
							_ -> return ()
						modifyIORef pagesRef tail
						return True
					_ -> return False
			_ -> return True
	waitField f

mkSVGFileName :: Maybe String -> Int -> Maybe String
mkSVGFileName (Just bfn) n = Just $ bfn ++ addZero (show n) ++ ".svg"
	where
	addZero s = replicate (2 - length s) '0' ++ s
mkSVGFileName _ _ = Nothing

titlePage :: Turtle -> IO ()
titlePage t = do
	writeTitle t title
	writeRB t author

title, author :: String
title = "Haskell入門"
author = "重城 良国"

pages :: [Turtle -> IO ()]
pages = [
	titlePage, what1, what2, what3, what4, what5, what6, what7,
	pure1 0,
	function1, function2,
	functionCheck1, functionCheck2, functionCheck3, functionCheck4,
	functionCheck5, functionCheck6, functionCheck7, functionCheck8,
	functionCheck9, functionCheck10,
	pure1 1,
	firstclass1, firstclass2, firstclass3, firstclass4,
	pure1 2,
	pure1 3,
	pure1 4
 ]

what1 :: Turtle -> IO ()
what1 t = do
	clear t
	writeTopTitle t "Haskellとは何か?"

what2 :: Turtle -> IO ()
what2 t = do
	setheading t $ - 90
	forward t $ 10 * rt
	setx t $ width / 3
	image t "HaskellBCurry.jpg" (279 * rt / 2) (343 * rt / 2)
	forward t (343 * rt / 2)
	text t "Haskell Brooks Curry (1900.9.12 - 1982 9.1)"
	text t "アメリカの記号論理学者"
	text t "名前の由来はこの人"

what3 :: Turtle -> IO ()
what3 t = do
	replicateM_ 23 $ undo t
	setx t $ width * 2 / 3
	image t "HaskellBCurry.jpg" (279 * rt / 2) (343 * rt / 2)
	semititle t "純粋関数型言語"
	text t "* 第一級関数"
	text t "* 参照透過"
	text t "* 静的型付"
	text t "* 遅延評価"

what4 :: Turtle -> IO ()
what4 t = do
	backward t $ 50 * rt
	dvArrow t
	text t "概念の本質的な部分をそのまま表現できる"
	text t ""

what5, what6, what7 :: Turtle -> IO ()
what5 t = text t "例: 小さい方から10個の素数が欲しい"
what6 t = text t "=> すべての素数を求める"
what7 t = text t "-> 小さい方から10個取り出す"

pure1 :: Int -> Turtle -> IO ()
pure1 n t = do
	clear t
	writeTopTitle t "純粋関数型言語"
	(if n == 1 then withRed t else id) $ semititle t "* 第一級関数"
	(if n == 2 then withRed t else id) $ semititle t "* 参照透過"
	(if n == 3 then withRed t else id) $ semititle t "* 静的型付"
	(if n == 4 then withRed t else id) $ semititle t "* 遅延評価"

withRed :: Turtle -> IO a -> IO a
withRed t act = do
	pencolor t "red"
	r <- act
	pencolor t "black"
	return r

function1 :: Turtle -> IO ()
function1 t = do
	clear t
	writeTopTitle t "関数とは?"

function2 :: Turtle -> IO ()
function2 t = do
	text t ""
	text t "0個以上の入力値をひとつの出力値へ変えるルール"
	goto t (width * 1 / 10) (height * 5 / 10)
	mkFunGraph t

functionCheck1, functionCheck2, functionCheck3, functionCheck4, functionCheck5
	:: Turtle -> IO ()
functionCheck1 t = do
	clear t
	writeTopTitle t "関数とは?(練習問題)"
	semititle t "以下の「関数」の入力と出力を述べよ"

functionCheck2 t = text t "足し算"
functionCheck3 t = text t "翻訳"
functionCheck4 t = text t "2"
functionCheck5 t = text t "与えられた文字列を表示する機能"
functionCheck6 t = text t "" >> text t "答え"
functionCheck7 t = text t "足し算: 数 -> 数 -> 数"
functionCheck8 t = text t "翻訳: ある言語の文 -> 別の言語の文"
functionCheck9 t = text t "2: 0個の入力を取り、出力として数を返す"
functionCheck10 t = text t "与えられた文字列を表示する機能: 文字列 -> 動作"

mkFunGraph :: Turtle -> IO ()
mkFunGraph t = do
	write t fontName semiBigF "入力1"
	setheading t $ - 90
	forward t (height * 1 / 5)
	write t fontName semiBigF "入力2"
	backward t (height * 1 / 5 + semiBigF / 2)
	left t 90
	forward t (width * 5 / 40)
	x <- xcor t
	arrow t (width * 1 / 10)
	setheading t $ - 90
	forward t (height * 1 / 5)
	left t 90
	setx t x
	arrow t (width * 1 / 10)
	setheading t 0
	goto t (width * 135 / 364) (height * 2 / 5)
	pensize t 2
	pendown t
	replicateM_ 2 $ do
		forward t (width * 1 / 4)
		right t 90
		forward t (height * 7 / 20)
		right t 90
	penup t
	goto t (width * 13 / 20) (height * 23 / 40)
	pendown t
	arrow t (width * 1 / 10)
	setheading t (- 90)
	forward t $ semiBigF / 2
	left t 90
	forward t (12 * rt)
	write t fontName semiBigF "出力"

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

firstclass1 :: Turtle -> IO ()
firstclass1 t = do
	clear t
	writeTopTitle t "第一級関数とは?"

firstclass2 :: Turtle -> IO ()
firstclass2 t = do
	text t "関数が第一級オブジェクトであるということ"

firstclass3 :: Turtle -> IO ()
firstclass3 t = do
	writeNextTitle t "第一級オブジェクトとは?"

firstclass4 :: Turtle -> IO ()
firstclass4 t = do
	text t "* 関数の引数になれる"
	text t "* 関数の返り値になれる"
	text t "* 変数に格納可能である"
	text t "* データ構造に格納可能である"

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

writeNextTitle :: Turtle -> String -> IO ()
writeNextTitle t ttl = do
	let sz = bigF
	setx t $ (width - sz * myLength ttl) / 2
	setheading t $ -90
	forward t $ sz * 2
	write t fontName sz ttl
	left t 90
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
