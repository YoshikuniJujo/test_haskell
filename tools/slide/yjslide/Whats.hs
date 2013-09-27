module Whats (
	whats1, whats2, whats3, whats4
) where

import Graphics.X11.Turtle
import Lecture

whatTitleStr :: String
whatTitleStr = "Haskellとは何か?"

whatTitle :: Turtle -> IO ()
whatTitle t = do
	flushoff t
	writeTopTitle t whatTitleStr
	writeImageRight t haskellBCurry
	flushon t

whats1 :: [Turtle -> IO ()]
whats1 = [what1, what2]

what1, what2 :: Turtle -> IO ()
what1 t = writeTopTitle t whatTitleStr
what2 t = do
	writeImageCenter t haskellBCurry
	setheading t $ - 90
	text t "Haskell Brooks Curry (1900.9.12 - 1982 9.1)"
	text t "アメリカの記号論理学者"
	text t "名前の由来はこの人"

whats2 :: [Turtle -> IO ()]
whats2 = [what3_1, what3_2, what3_3, what3_4]

what3_1, what3_2, what3_3, what3_4 :: Turtle -> IO ()
what3_1 t = whatTitle t >> text t "遅延評価する関数型言語の乱立"
what3_2 t = do
	dvArrowShort t
	text t "1990年 標準としてのHaskell 1.0"
what3_3 t = do
	dvArrowShort t
	text t "Haskell 98、Haskell'、Haskell 2010"
	itext t 4 "と進化"
what3_4 t = do
	dvArrowShort t
	text t "ghc(代表的な処理系)内での拡張機能として進化は続く"
	arrowIText t 1 "十分に吟味されたものが次の標準に取り込まれる"

whats3 :: [Turtle -> IO ()]
whats3 = [what4, what5, what6, what7, what7_5]
what4, what5, what6, what7, what7_5 :: Turtle -> IO ()
what4 t = whatTitle t >> text t "研究者の努力の結晶"
what5 t = do
	dvArrowShort t
	text t "Haskellを学ぶということは"
	itext t 1 "彼らの成果を刈り取ること"
	text t ""
what6 t = text t "難しい理論の理解が必要?"
what7 t = do
	dvArrowShort t
	text t "難しい理論は「利用者が簡単に使う」ためにある"
what7_5 t = do
	itext t 1 "レゴブロックを使うのにひとつひとつのブロックの"
	itext t 1 "作りかたを知る必要はない"

whats4 :: [Turtle -> IO ()]
whats4 = [\t -> do
	whatTitle t >> semititle t "何ができるの?", \t -> do
	text t "* たいていのことはできる", \t -> do
	text t "* FFIという機能でCの関数が使える", \t -> do
	arrowIText t 0 "実用的な言語"
	text t "", \t -> do
	semititle t "他の言語を使い続けるとしても", \t -> do
	text t "* Haskellを学ぶことによって"
	itext t 1 "得られる様々な抽象化の手法は使える", \t -> do
	text t "* プログラミングに本質的な様々な概念を"
	itext t 1 "新たな光のもとに別の視点から見ることができる"]
