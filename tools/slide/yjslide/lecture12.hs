module Main where

import Lecture

subtitle :: String
subtitle = "第12回 ランダム"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, randomTypes, stdGen, randoms, randomR, invariable,
	getStdGen, useGetStdGen
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ランダムな値が欲しくなることがある", \t -> do
	itext t 1 "- もちろんコンピュータで作れるのは疑似乱数", \t -> do
	text t "* 講義の取み立て上、今回ランダムについて扱う", \t -> do
	itext t 1 "- 「第13回 テスト」のQuickCheckで使うので", \t -> do
	text t "* System.Randomモジュールを使う"
	text t "", \t -> do
	text t "* ランダムな値を得るためには種が必要", \t -> do
	text t "* 次のランダム値を得るためには新しい種が必要", \t -> do
	text t "* よって関数randomの型は以下のようになる"
	itext t 1 "random :: g -> (a, g)"
 ]

randomTypes :: Page
randomTypes = [\t -> do
	writeTopTitle t "型について"
	text t "", \t -> do
	text t "* 本当は以下のようになっている"
	text t "random :: (RandomGen g, Random a) => g -> (a, g)"
	text t "", \t -> do
	text t "* RandomGenクラス", \t -> do
	itext t 1 "- ランダム関数の種はこのクラスに所属する値", \t -> do
	itext t 1 "- 種の型によって生成アルゴリズムを変えられる"
	text t "", \t -> do
	text t "* Randomクラス", \t -> do
	itext t 1 "- このクラスに属する型の乱数を生成できる"
 ]

stdGen :: Page
stdGen = [\t -> do
	writeTopTitle t "StdGen"
	text t "", \t -> do
	text t "* 種としてはじめから用意されている型", \t -> do
	text t "* 特に問題がなければこれを使っておけば良い", \t -> do
	text t "* 整数から種を作ることができる", \t -> do
	itext t 1 "mkStdGen :: Int -> StdGen", \t -> do
	itext t 1 "> mkStdGen 8"
	itext t 1 "9 1"
	text t "", \t -> do
	text t "* これで乱数の生成ができる"
	itext t 1 "> random (mkStdGen 8) :: (Int, StdGen)"
	itext t 1 "(-158441753, 1525180386 1655838864)"
 ]

randoms :: Page
randoms = [\t -> do
	writeTopTitle t "randoms"
	text t "", \t -> do
	text t "* 次々と乱数を生成していくためには", \t -> do
	itext t 1 "- 種をつくる", \t -> do
	itext t 1 "- randomにその種を与える", \t -> do
	itext t 1 "- ランダムな値と新しい種を得る", \t -> do
	itext t 1 "- randomにその種を与える", \t -> do
	itext t 1 "- くりかえす", \t -> do
	dvArrowShort t
	text t "もっと簡単にできる", \t -> do
	arrowIText t 1 "randomsを使えば乱数列の無限リストが得られる", \t -> do
	itext t 1 "> take 10 $ randoms (mkStdGen 8) :: [Int]"
	itext t 1 "[-158441753, -1554673138, 1183211287, ..."
 ]

randomR :: Page
randomR = [\t -> do
	writeTopTitle t "randomR"
	text t "", \t -> do
	text t "* 値の範囲を指定したい", \t -> do
	text t "randomR :: (a, a) -> g -> (a, g)", \t -> do
	text t "> randomR (1, 6) (mkStdGen 8)"
	text t "(6, 360126 40692)"
	text t "", \t -> do
	text t "> randomRs :: (a, a) -> g -> [a]", \t -> do
	text t "> take 10 $ randomRs (1, 6) (mkStdGen 8)"
	text t "[6, 6, 3, 4, 2, 2, 2, 6, 3, 4]"
 ]

invariable :: Page
invariable = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* 必ず決まった値が得られるランダム値について学んだ", \t -> do
	text t "* 同じ種からは常に同じ乱数列が得られる", \t -> do
	itext t 1 "- シューティングゲーム等に向いた性質", \t -> do
	itext t 1 "- おみくじ等には向かない", \t -> do
	dvArrowShort t
	text t "おみくじ等に向くランダム値について見ていこう"
 ]

getStdGen :: Page
getStdGen = [\t -> do
	writeTopTitle t "getStdGen"
	text t "", \t -> do
	text t "* 起動ごとに異なる種を用意", \t -> do
	itext t 1 "getStdGen :: IO StdGen", \t -> do
	itext t 1 "> getStdGen"
	itext t 1 "1431823557 1"
	itext t 1 "> getStdGen"
	itext t 1 "1431823557 1", \t -> do
	text t "* 同じプロセス中では同じ値が返る", \t -> do
	text t "* ghcの実装では内部的に時刻を使用している"
 ]

useGetStdGen :: Page
useGetStdGen = [\t -> do
	writeTopTitle t "getStdGenの使用例"
	text t "", \t -> do
	text t "main :: IO ()"
	text t "main = do"
	itext t 1 "g <- getStdGen"
	itext t 1 "print $ take 10 $ randomRs (1 :: Int, 6) g", \t -> do
	dvArrowShort t
	text t "試行1: [2, 2, 4, 6, 4, 3, 6, 2, 1, 4]", \t -> do
	text t "試行2: [1, 1, 2, 6, 5, 3, 1, 4, 5, 3]"
 ]
