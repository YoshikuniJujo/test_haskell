module Main where

import Lecture

subtitle :: String
subtitle = "第14回 モナド"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	monadLaws, monadLaw1, monadLaw2, monadLaw12, monadLaw3, monadLaw3',
	useMaybe, useMaybe2, useMaybe3
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* IOモナドの説明ではモナド自体の説明は省略", \t -> do
	itext t 1 "- IOモナドの本質はモナドではない", \t -> do
	itext t 1 "- モナドという方向からIOを見ると誤解が生じる", \t -> do
	itext t 1 "- IOは特殊なので他のモナドとは区別する必要がある"
	text t "", \t -> do
	text t "* モナドとは何か?", \t -> do
	itext t 1 "以下の型の関数を持つ型m", \t -> do
	itext t 1 "return :: a -> m a"
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	text t "今は理解できなくていい", \t -> do
	arrowIText t 1 "モナドの理解はじわじわと深めていけば良い"
 ]

monadLaws :: Page
monadLaws = [\t -> do
	writeTopTitle t "モナド則(前置き)"
	text t "", \t -> do
	text t "* モナドには3つの基本則がある", \t -> do
	itext t 1 "1. return x >>= f == f x", \t -> do
	itext t 1 "2. m >>= return == m", \t -> do
	itext t 1 "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)"
	text t "", \t -> do
	text t "これらについて簡単に説明していこう", \t -> do
	itext t 1 "- 今はわからなくても良い", \t -> do
	itext t 1 "- 自分でオリジナルなモナドを作るときに必要", \t -> do
	itext t 1 "- 何となくそういう決まりがある程度の理解でOK"
 ]

monadLaw1 :: Page
monadLaw1 = [\t -> do
	writeTopTitle t "モナド則1"
	text t "", \t -> do
	semititle t "1. return x >>= f == f x", \t -> do
	text t "* do記法で書いてみる"
	itext t 1 "do"
	preLine t
	itext t 2 "y <- return x"
	itext t 2 "f y", \t -> do
	text t "* return xは「何もせずにxをmに包み込む」ということ", \t -> do
	text t "* 「何もせずに包み込まれたx」を取り出してfを適用する", \t -> do
	text t "* xにfを適用するということ", \t -> do
	dvArrowShort t
	text t "returnで包み込まれた値はそのまま取り出せることを保証"
 ]

monadLaw2 :: Page
monadLaw2 = [\t -> do
	writeTopTitle t "モナド則2"
	text t "", \t -> do
	semititle t "2. m >>= return == m", \t -> do
	text t "* do記法で書いてみる"
	itext t 1 "do"
	preLine t
	itext t 2 "x <- m"
	itext t 2 "return x", \t -> do
	text t "* 何かをした結果包み込まれた値を取り出す", \t -> do
	text t "* その値を「何もせずに包み込む」", \t -> do
	text t "* 取り出したものを包み込んだだけ", \t -> do
	text t "* もともとの結果と同じこと", \t -> do
	dvArrowShort t
	text t "* returnが包み込まれた値を変えないことを保証", \t -> do
	text t "* returnが包み込みかたを変えないことを保証"
 ]

monadLaw12 :: Page
monadLaw12 = [\t -> do
	writeTopTitle t "モナド則1と2"
	text t "", \t -> do
	semititle t "1. return x >>= f == f x", \t -> do
	semititle t "2. m >>= return = m", \t -> do
	text t "* returnで包み込まれた値はそのまま取り出せる", \t -> do
	text t "* returnは包み込まれた値を変えない"
	text t "* returnは包み込みかたを変えない", \t -> do
	dvArrowShort t
	text t "returnが包み込む以外に「何もしない」ことを保証している"
 ]

monadLaw3 :: Page
monadLaw3 = [\t -> do
	writeTopTitle t "モナド則3"
	text t "", \t -> do
	semititle t "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)", \t -> do
	text t "* do記法で書いてみる"
	itext t 1 "do"
	preLine t
	itext t 1.5 "y <- do"
	preLine t
	itext t 3 "x <- m"
	itext t 3 "f x"
	itext t 1.5 "g y", \t -> do
	preLine t
	preLine t
	preLine t
	itext t 5 "do"
	preLine t
	itext t 5.5 "x <- m"
	itext t 5.5 "do"
	preLine t
	itext t 6 "y <- f x"
	itext t 6 "g y", \t -> do
	text t "* mから取り出した値をfに適用したもの"
	text t "* そこから取り出した値をgに適用"
	text t "", \t -> do
	text t "* mから取り出した値"
	text t "* それをfに適用したものから取り出した値にgを適用"
 ]

monadLaw3' :: Page
monadLaw3' = [\t -> do
	writeTopTitle t "モナド則3"
	text t ""
	semititle t "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)", \t -> do
	text t "* この法則は(x + y) + z == x + (y + z)と類似", \t -> do
	text t "* つまり結合則ということ", \t -> do
	text t "* mとfを先に結合しても、fとgを先に結合しても結果は同じ"
	text t "", \t -> do
	text t "自分でモナドを作るとき", \t -> do
	dvArrowShort t
	semititle t "3つのモナド則を満たすように気をつけよう"
	text t "", \t -> do
	text t "...ここまでは前置き"
 ]

useMaybe :: Page
useMaybe = [\t -> do
	writeTopTitle t "Maybeを使う"
	text t "", \t -> do
	text t "* ここからが本番", \t -> do
	text t "* 失敗する可能性のある関数を考える", \t -> do
	text t "* ある値がリストの何番目にあるか調べる関数を考えよう", \t -> do
	itext t 1 "elemIndex1 :: Eq a => Int -> a -> [a] -> Int"
	itext t 1 "elemIndex1 n x0 (x : xs)"
	itext t 2 "| x0 == x = n"
	itext t 2 "| otherwise = elemIndex1 (n + 1) xs"
	itext t 1 "elemIndex1 _ _ [] = error \"not exist\"", \t -> do
	arrowIText t 1 "リストに存在しない値を探した場合異常終了", \t -> do
	arrowIText t 1 "うれしくない"
 ]

useMaybe2 :: Page
useMaybe2 = [\t -> do
	writeTopTitle t "Maybeを使う"
	text t "", \t -> do
	text t "* 値が存在していたならばその位置を返す", \t -> do
	text t "* 存在しなければ存在しなかったことを示す値を返す", \t -> do
	text t "* そういう型が欲しい", \t -> do
	itext t 1 "data MaybeInt = JustInt Int | NoInt", \t -> do
	text t "* もっと一般的な型を定義できる", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing"
 ]

useMaybe3 :: Page
useMaybe3 = [\t -> do
	writeTopTitle t "Maybeを使う"
	text t "", \t -> do
	text t ""
 ]
