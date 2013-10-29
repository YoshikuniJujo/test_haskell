module Main where

import Lecture

subtitle :: String
subtitle = "第12回 ランダム"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, randomTypes, stdGen, randoms, randomR, invariable,
	getStdGen, useGetStdGen, setStdGen, ios, variable,
	split, newStdGen, splitSummary,
	instanceRandom, instanceRandomPol, instanceRandomPol2,
	instanceRandomPolUse, instanceRandomSummary, summary
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

setStdGen :: Page
setStdGen = [\t -> do
	writeTopTitle t "setStdGen"
	text t "", \t -> do
	text t "* 種の値を更新する", \t -> do
	itext t 1 "setStdGen :: StdGen -> IO ()", \t -> do
	itext t 1 "> g <- getStdGen"
	itext t 1 "> random g :: (Int, StdGen)"
	itext t 1 "> setStdGen $ snd it", \t -> do
	dvArrowShort t
	text t "この流れをまとめてしてくれる関数がある", \t -> do
	itext t 1 "getStdRandom :: (StdGen -> (a, StdGen)) -> IO a", \t -> do
	itext t 1 "> getStdGen => 2049318214 1"
	itext t 1 "> getStdRandom random"
	itext t 1 "> getStdGen => 1008136518 1655838864"
 ]

ios :: Page
ios = [\t -> do
	writeTopTitle t "randomIO, randomRIO"
	text t "", \t -> do
	text t "* より簡単に使える関数"
	itext t 1 "randomIO :: IO a"
	itext t 1 "randomIO = getStdRandom random", \t -> do
	itext t 1 "randomRIO :: (a, a) -> IO a"
	itext t 1 "randomRIO r = getStdRandom $ randomR r", \t -> do
	text t "> randomIO "
	text t "1194332419"
	text t "> randomIO"
	text t "923507800"
	text t "> randomRIO (1, 6)"
	text t "5"
 ]

variable :: Page
variable = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* 起動ごとに異なる乱数値を得るにはgetStdGenを使う", \t -> do
	text t "* 保存されている種の更新にはsetStdGenを使う", \t -> do
	text t "* getStdRandomを使えば値の入手と種の更新を同時にできる", \t -> do
	itext t 1 "- マルチスレッドで使う場合はこっちを使うべき", \t -> do
	text t "* 使いやすいrandomIOとrandomRIOが用意されている"
 ]

split :: Page
split = [\t -> do
	writeTopTitle t "split"
	text t "", \t -> do
	text t "* ランダムの種を2つに分けることができる", \t -> do
	itext t 1 "split :: g -> (g, g)", \t -> do
	itext t 1 "> g <- getStdGen"
	itext t 1 "> let (g1, g2) = split g"
	itext t 1 "> take 10 $ randoms g1 :: [Int]"
	itext t 1 "> take 10 $ randoms g2 :: [Int]"
 ]

newStdGen :: Page
newStdGen = [\t -> do
	writeTopTitle t "newStdGen"
	text t "", \t -> do
	text t "* ランダムの種を2つに分けて片方を保存し片方を返す関数", \t -> do
	itext t 1 "newStdGen :: IO StdGen", \t -> do
	itext t 1 "> g1 <- newStdGen"
	itext t 1 "> g2 <- newStdGen"
	itext t 1 "> g3 <- newStdGen"
	itext t 1 "> take 10 $ randoms g1 :: [Int]"
	itext t 1 "> take 10 $ randoms g2 :: [Int]"
	itext t 1 "> take 10 $ randoms g3 :: [Int]", \t -> do
	text t "* getStdGenだとまるごと取ってきちゃう感じ", \t -> do
	text t "* newStdGenだとすこしずつ削って使う感じ"
 ]

splitSummary :: Page
splitSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* ランダムの種は2つに分けることができる", \t -> do
	text t "* 複数の系列のランダム値を使いたいときに便利", \t -> do
	text t "* newStdGenを使えば新しい種を何度でも入手可能", \t -> do
	text t "* getStdGenはまるごと取ってくる感じ", \t -> do
	text t "* newStdGenはすこしずつ削って使う感じ"
 ]

instanceRandom :: Page
instanceRandom = [\t -> do
	writeTopTitle t "自作の型をランダムで使う"
	text t "", \t -> do
	text t "* Randomクラスのインスタンスにする", \t -> do
	arrowIText t 1 "今まで紹介してきた関数がその型の上で使える", \t -> do
	text t "* randomRとrandomを定義すれば良い"
	text t "", \t -> do
	text t "極座標の例:", \t -> do
	text t "data Pol = Pol Double Double"
 ]

instanceRandomPol :: Page
instanceRandomPol = [\t -> do
	writeTopTitle t "極座標をランダムで入手"
	text t "", \t -> do
	text t "instance Random Pol where"
	itext t 1 "random g = randomR (Pol 0 0) (Pol 100 (2 * pi))"
	itext t 1 "randomR (Pol dmin amin, Pol dmax amax) g ="
	itext t 2 "(d, g') = randomR (dmin, dmax) g"
	itext t 2 "(a, g'') = randomR (amin, amax) g' in"
	itext t 2 "(Pol d a, g'')"
 ]

instanceRandomPol2 :: Page
instanceRandomPol2 = [\t -> do
	writeTopTitle t "極座標をランダムで入手"
	text t "", \t -> do
	text t "random g = randomR (Pol 0 0) (Pol 100 (2 * pi)) ", \t -> do
	itext t 1 "- 次に定義するrandomRを使っている", \t -> do
	itext t 1 "- 距離はとりあえず0から100まで", \t -> do
	itext t 1 "- 角度は0から2\x03c0まで", \t -> do
	text t "randomR (Pol dmin amin, Pol dmax amax) g =", \t -> do
	itext t 1 "(d, g') = randomR (dmin, dmax) g", \t -> do
	itext t 2 "- 距離はdminからdmaxの範囲のランダム値", \t -> do
	itext t 1 "(a, g'') = randomR (amin, amax) g' in", \t -> do
	itext t 2 "- 角度はaminからamaxの範囲のランダム値", \t -> do
	itext t 1 "(Pol d a, g'')"
 ]

instanceRandomPolUse :: Page
instanceRandomPolUse = [\t -> do
	writeTopTitle t "極座標をランダムで入手"
	text t "", \t -> do
	text t "> randomIO :: IO Pol"
	text t "Pol 61.15303936941166 4.251066312657999", \t -> do
	text t "> randomIO :: IO Pol"
	text t "Pol 80.0939538435434 3.0725675489984137", \t -> do
	text t "> g <- getStdGen", \t -> do
	text t "take 10 $ randoms g :: [Pol]"
	text t "[Pol 82.5146420164557 5.390267779550682, ... ]"
 ]

instanceRandomSummary :: Page
instanceRandomSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* ランダム値が欲しければRandomクラスのインスタンスに", \t -> do
	text t "* randomとrandomRを定義すれば良い", \t -> do
	text t "* その型を構成する型のrandomやrandomRを使えば簡単"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* ランダム値の使いかたを学んだ", \t -> do
	text t "* 種を指定すれば決まった値を得ることができる", \t -> do
	text t "* システムが提供する種で起動ごとに違う乱数", \t -> do
	text t "* 種を2つに分けることができる", \t -> do
	itext t 1 "- 複数の乱数列が作れる", \t -> do
	text t "* 自分で作った型も乱数として扱うことができる"
 ]
