module Main where

import Lecture

subtitle :: String
subtitle = "第17回 モナド変換子"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	examErrorState, examErrorState2, examErrorState3
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 「第15回 いろいろなモナド」でいろいろなモナドを見た", \t -> do
	text t "* それらのモナドを組み合わせて使いたくなることがある", \t -> do
	itext t 1 "- 失敗する可能性のある状態を取る計算", \t -> do
	itext t 1 "- 環境を持ちログを記録する計算", \t -> do
	itext t 1 "- などなど", \t -> do
	text t "* IOモナドと組み合わせて使いたくなることもある"
 ]

examErrorState :: Page
examErrorState = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* 失敗の可能性のある計算は以下のようになる", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing"
	itext t 1 "return = Just"
	itext t 1 "Just x >>= f = f x"
	itext t 1 "Nothing >>= f = Nothing", \t -> do
	text t "* 状態を取る計算は以下のようになる", \t -> do
	itext t 1 "data State s a = State { runState :: s -> (a, s) }"
	itext t 1 "return a = State $ \\s -> (a, s)"
	itext t 1 "(State x) >>= f = State $ \\s ->"
	itext t 2 "let (v, s') = x s in runState (f v) s'"
 ]

examErrorState2 :: Page
examErrorState2 = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* それぞれの型の定義をよく見る", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing"
	itext t 1 "data State s a = State { runState :: s -> (a, s) }", \t -> do
	text t "* この2つを合わせた型を作ってみよう", \t -> do
	itext t 1 "data StateMaybe = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }"
 ]

examErrorState3 :: Page
examErrorState3 = [\t -> do
	writeTopTitle t "失敗と状態のある計算"
	text t "", \t -> do
	text t "* モナドにする", \t -> do
	itext t 1 "data StateMaybe = StateMaybe {"
	itext t 2 "runStateMaybe :: s -> Maybe (a, s) }", \t -> do
	itext t 1 "return a = StateMaybe $ \\s -> Just (a, s)"
	itext t 1 "StateMaybe x >>= f = StateMaybe $ \\s ->"
	itext t 2 "case x s of"
	itext t 3 "Just (v, s') -> runStateMaybe (f v) s'"
	itext t 3 "_ -> Nothing"
 ]

preludeMonadsTf :: Page
preludeMonadsTf = [\t -> do
	text t "* そういったことを行うためのパッケージが用意されている", \t -> do
	itext t 1 "- パッケージは「第28回 cabalの使いかた」で", \t -> do
	itext t 1 "- パッケージの名前はmonads-tf"
 ]
