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
	setx t (width / 8)
	flushon t

whats1 :: [Turtle -> IO ()]
whats1 = [\t -> do
	writeTopTitle t whatTitleStr, \t -> do
	writeImageCenter t 85 haskellBCurry
	setheading t $ - 90, \t -> do
	text t "Haskell Brooks Curry (1900.9.12 - 1982 9.1)", \t -> do
	itext t 1 "アメリカの記号論理学者", \t -> do
	itext t 1 "名前の由来はこの人"
 ]

whats2 :: [Turtle -> IO ()]
whats2 = [\t -> do
	whatTitle t, \t -> do
	text t "遅延評価する関数型言語の乱立", \t -> do
	dvArrowShort t
	text t "1990年 標準としてのHaskell 1.0", \t -> do
	dvArrowShort t
	text t "Haskell 98、Haskell'、Haskell 2010"
	itext t 4 "と進化", \t -> do
	dvArrowShort t
	text t "ghc(代表的な処理系)内での拡張機能として進化は続く", \t -> do
	arrowIText t 1 "十分に吟味されたものが次の標準に取り込まれる"
 ]

whats3 :: [Turtle -> IO ()]
whats3 = [\t -> do
	whatTitle t, \t -> do
	text t "研究者の努力の結晶", \t -> do
	dvArrowShort t
	text t "Haskellを学ぶということは"
	itext t 1 "彼らの成果を刈り取ること"
	text t "", \t -> do
	text t "難しい理論の理解が必要?", \t -> do
	dvArrowShort t
	text t "難しい理論は「利用者が簡単に使う」ためにある", \t -> do
	itext t 1 "レゴブロックを使うのにひとつひとつのブロックの"
	itext t 1 "作りかたを知る必要はない"
 ]

whats4 :: [Turtle -> IO ()]
whats4 = [\t -> do
	whatTitle t, \t -> do
	semititle t "何ができるの?", \t -> do
	text t "* たいていのことはできる", \t -> do
	text t "* FFIという機能でCの関数が使える", \t -> do
	arrowIText t 0 "実用的な言語"
	text t "", \t -> do
	semititle t "他の言語を使い続けるとしても", \t -> do
	text t "* Haskellを学ぶことによって"
	itext t 1 "得られる様々な抽象化の手法は使える", \t -> do
	text t "* プログラミングに本質的な様々な概念を"
	itext t 1 "新たな光のもとに別の視点から見ることができる"]
