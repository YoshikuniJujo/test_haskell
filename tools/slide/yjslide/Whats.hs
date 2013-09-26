module Whats (
	picture, whats1, whats2, whats3, whats4
) where

import Graphics.X11.Turtle
import Lecture

picture :: (Double, Double, FilePath)
picture = (139.5, 171.5, "HaskellBCurry.jpg")

whatTitleStr :: String
whatTitleStr = "Haskellとは何か?"

whatTitle :: Turtle -> IO ()
whatTitle t = do
	flushoff t
	writeTopTitle t whatTitleStr
	writeImageRight t picture
	flushon t

whats1 :: [Turtle -> IO ()]
whats1 = [what1, what2]

what1, what2 :: Turtle -> IO ()
what1 t = writeTopTitle t whatTitleStr
what2 t = do
	writeImageCenter t picture
	setheading t $ - 90
	text t "Haskell Brooks Curry (1900.9.12 - 1982 9.1)"
	text t "アメリカの記号論理学者"
	text t "名前の由来はこの人"

whats2 :: [Turtle -> IO ()]
whats2 = [what3_1, what3_2, what3_3, what3_4]

what3_1, what3_2, what3_3, what3_4 :: Turtle -> IO ()
what3_1 t = whatTitle t >> text t "遅延評価する関数型言語の乱立"
what3_2 t = do
	setx t $ width / 3
	dvLArrow t 12
	text t "1990年 標準としてのHaskell 1.0"
what3_3 t = do
	setx t $ width / 3
	dvLArrow t 12
	text t "Haskell 98、Haskell'、Haskell 2010"
	itext t 4 "と進化"
what3_4 t = do
	setx t $ width / 3
	dvLArrow t 12
	text t "ghc(代表的な処理系)内での拡張機能として進化は続く"
	y <- ycor t
	setx t $ width * 5 / 32
	setheading t $ - 90
	forward t $ normalF * 3 / 2
	left t 90
	arrow t $ width / 28
	sety t y
	itext t 1 "十分に吟味されたものが次の標準に取り込まれる"

whats3 :: [Turtle -> IO ()]
whats3 = [what4, what5, what6, what7, what7_5]
what4, what5, what6, what7, what7_5 :: Turtle -> IO ()
what4 t = whatTitle t >> text t "研究者の努力の結晶"
what5 t = do
	setx t $ width / 3
	dvLArrow t 12
	text t "Haskellを学ぶということは"
	itext t 1 "彼らの成果を刈り取ること"
	text t ""
what6 t = text t "難しい理論の理解が必要?"
what7 t = do
	setx t $ width / 3
	dvLArrow t 12
	text t "難しい理論は「利用者が簡単に使う」ためにある"
what7_5 t = do
	itext t 1 "レゴブロックを使うのにひとつひとつのブロックの"
	itext t 1 "作りかたを知る必要はない"

whats4 :: [Turtle -> IO ()]
whats4 = [what7_6, what7_7, what7_8, what7_9, what8_1, what8_2, what8_3]

what7_6, what7_7, what7_8, what7_9, what8_1, what8_2, what8_3
	:: Turtle -> IO ()
what7_6 t = whatTitle t >> semititle t "何ができるの?"
what7_7 t = text t "* C言語にできることは何でも"
what7_8 t = do
	itext t 0.5 "FFIという機能でCの関数が使える"
	setheading t $ -90
	forward t $ normalF * 13 / 8
	left t 90
	setx t $ width / 8
what7_9 t = do
	arrow t $ width / 20
	left t 90
	forward t $ normalF * 13 / 8
	itext t 1 "実用的な言語"
	text t ""
what8_1 t = semititle t "他の言語を使い続けるとしても"
what8_2 t = do
	text t "* Haskellを学ぶことによって"
	itext t 1 "得られる様々な抽象化の手法は使える"
what8_3 t = do
	text t "* プログラミングに本質的な様々な概念を"
	itext t 1 "新たな光のもとに別の視点から見ることができる"
