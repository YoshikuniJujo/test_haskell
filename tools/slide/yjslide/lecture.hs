module Main where

import Lecture
import Graphics.X11.Turtle

main :: IO ()
main = runLecture pages

title, subtitle, author :: String
title = "Haskell入門"
subtitle = "第1回 Haskellの特徴"
author = "重城 良国"

picture :: (Double, Double, FilePath)
picture = (139.5, 171.5, "HaskellBCurry.jpg")

pages :: [Turtle -> IO ()]
pages = concat [
	[titlePage], whats1, whats2, whats3, whats4, whats5,
	[pure1 0], functions1, functionChecks1,
	[pure1 1], firstclasses1, firstclassExams1, syntaxes1,
		higherOrders1, higherOrders2, higherOrderChecks1,
	[pure1 2], transparencies1, transparencies2,
	[pure1 3], whatIsTypes1, whatIsTypes2, whatIsTypeChecks1,
		staticTypings1, typeFlexibilities1,
	[pure1 4], lazyEvaluations1, lazyEvaluations2, lazyEvaluationChecks1,
	summaries1]

titlePage :: Turtle -> IO ()
titlePage t = writeTitle t title subtitle author

whatTitleStr :: String
whatTitleStr = "Haskellとは何か?"

whatTitle :: Turtle -> IO ()
whatTitle t = do
	flushoff t
	writeTopTitle t whatTitleStr
	writeImage t (width * 2 / 3) False picture
	flushon t

whats1 :: [Turtle -> IO ()]
whats1 = [what1, what2]

what1, what2 :: Turtle -> IO ()
what1 t = writeTopTitle t whatTitleStr
what2 t = do
	writeImage t (width / 3) True picture
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

whats5 :: [Turtle -> IO ()]
whats5 = [what9, what10, what11, what12, what13]

what9, what10, what11, what12, what13 :: Turtle -> IO ()
what9 t = do
	writeTopTitle t "Haskellの特徴"
	text t "純粋関数型言語であり"
	itext t 1 "* 第一級関数"
	itext t 1 "* 参照透過性"
	itext t 1 "* 静的型付け"
	itext t 1 "* 遅延性"
	text t "という特徴を持つ"
what10 t = do
	backward t $ width / 10
	dvLArrow t 12
	text t "概念の本質的な部分をそのまま表現できる"
	text t ""
what11 t = text t "例: 小さい方から10個の素数が欲しい"
what12 t = text t "=> すべての素数を求める"
what13 t = text t "-> 小さい方から10個取り出す"

pure1 :: Int -> Turtle -> IO ()
pure1 n t = do
	flushoff t
	hideturtle t
	clear t
	writeTopTitle t "Haskellの特徴"
	speed t "fastest"
	(if n == 0 then withRed t else id) $ semititle t "純粋関数型言語"
	(if n == 1 then withRed t else id) $ semititle t "* 第一級関数"
	(if n == 2 then withRed t else id) $ semititle t "* 参照透過性"
	(if n == 3 then withRed t else id) $ semititle t "* 静的型付け"
	(if n == 4 then withRed t else id) $ semititle t "* 遅延性"
	speed t "slow"
	flushon t

functions1 :: [Turtle -> IO ()]
functions1 = [function1, function2]

function1, function2 :: Turtle -> IO ()
function1 t = writeTopTitle t "関数とは?"
function2 t = do
	text t "0個以上の入力値をひとつの出力値へ変えるルール"
	goto t (width * 1 / 10) (height * 5 / 10)
	graphWrite t "入力1"
	setheading t $ - 90
	forward t (height * 1 / 5)
	graphWrite t "入力2"
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
	drawRect t (width / 4) (height * 7 / 20)
	goto t (width * 13 / 20) (height * 23 / 40)
	pendown t
	arrow t (width * 1 / 10)
	setheading t (- 90)
	forward t $ semiBigF / 2
	left t 90
	forward t normalF
	graphWrite t "出力"

functionChecks1 :: [Turtle -> IO ()]
functionChecks1 = [
	functionCheck1, functionCheck2, functionCheck3, functionCheck5,
	functionCheck6, functionCheck7, functionCheck8, functionCheck10]

functionCheck1, functionCheck2, functionCheck3, functionCheck5,
	functionCheck6, functionCheck7, functionCheck8, functionCheck10
	:: Turtle -> IO ()
functionCheck1 t = do
	writeTopTitle t "関数とは?(練習問題)"
	semititle t "以下の「関数」の入力と出力を述べよ"
functionCheck2 t = text t "足し算"
functionCheck3 t = text t "翻訳"
functionCheck5 t = text t "与えられた文字列を表示する機能"
functionCheck6 t = text t "" >> text t "答え"
functionCheck7 t = text t "足し算: 2つの数 -> 数"
functionCheck8 t = text t "翻訳: ある言語の文 -> 別の言語の文"
functionCheck10 t = text t "与えられた文字列を表示する機能: 文字列 -> 動作"

firstclasses1 :: [Turtle -> IO ()]
firstclasses1 = [firstclass1, firstclass2, firstclass3, firstclass4]

firstclass1, firstclass2, firstclass3, firstclass4 :: Turtle -> IO ()
firstclass1 t = writeTopTitle t "第一級関数とは?"
firstclass2 t = text t "関数が第一級オブジェクトであるということ"
firstclass3 t = writeNextTitle t "第一級オブジェクトとは?"
firstclass4 t = do
	text t "* リテラルとして表現できる"
	text t "* 変数に格納できる"
	text t "* データ構造に格納できる"
	text t "* 関数の引数になれる"
	text t "* 関数の返り値になれる"

firstclassExams1 :: [Turtle -> IO ()]
firstclassExams1 = [firstclassExam1, firstclassExam2, firstclassExam3,
	firstclassExam4, firstclassExam5]

firstclassExam1, firstclassExam2, firstclassExam3, firstclassExam4,
	firstclassExam5 :: Turtle -> IO ()
firstclassExam1 t = do
	writeTopTitle t "第一級関数とは?"
	text t "* リテラルとして表現できる"
	itext t 1 "\\x -> x * x"
firstclassExam2 t = do
	text t "* 変数に格納できる"
	itext t 1 "square = \\x -> x * x"
firstclassExam3 t = do
	text t "* データ構造に格納できる"
	itext t 1 "[\\x -> x * x]"
firstclassExam4 t = do
	text t "* 関数の引数になれる"
	itext t 1 "twice fun x = fun (fun x)"
	itext t 1 "twice sqrt 9 => 1.7320508075688772"
firstclassExam5 t = do
	text t "* 関数の返り値になれる"
	itext t 1 "addN n = \\x -> x + n"
	itext t 1 "(addN 3) 8 => 11"

syntaxes1 :: [Turtle -> IO ()]
syntaxes1 = [syntax1, syntax2, syntax3]

syntax1, syntax2, syntax3 :: Turtle -> IO ()
syntax1 t = do
	writeTopTitle t "ここまでに出てきた構文"
	text t "* 関数リテラル: \\parm -> expression"
	text t "* リストリテラル: [expression1, expression2, ... ]"
	text t "* 定義: var = expression"
	text t "* 関数定義: fun parm1 parm2 = expression"
	text t "* 関数適用: fun arg1 arg2"
	text t ""
syntax2 t = do
	text t "(注1) 変数の定義と0個の引数を取る関数の定義"
	itext t 1 "とは同じこと"
syntax3 t = do
	text t "(注2) 関数適用の結果を`=> value'のような"
	itext t 1 "形で示すが、これはHaskellの構文ではない。"

higherOrders1 :: [Turtle -> IO ()]
higherOrders1 = [higherOrder1, higherOrder2]

higherOrder1, higherOrder2 :: Turtle -> IO ()
higherOrder1 t = do
	writeTopTitle t "高階関数"
	text t "高階関数とは引数または返り値が関数であるような関数"
higherOrder2 t = do
	text t "つまり"
	text t ""
	text t "関数が第一級オブジェクトである"
	setx t $ width / 3
	dvArrow t
	text t "高階関数が書ける"
	text t ""
	text t "ということ"

higherOrders2 :: [Turtle -> IO ()]
higherOrders2 = [higherOrder3, higherOrder4, higherOrder5]

higherOrder3, higherOrder4, higherOrder5 :: Turtle -> IO ()
higherOrder3 t = do
	writeTopTitle t "高階関数"
	hideturtle t
	text t "高階関数とは引数または返り値が関数であるような関数"
	text t ""
	showturtle t
	text t "何がうれしいの?"
	text t ""
higherOrder4 t = do
	text t "* より高レベルな抽象化"
	itext t 1 "枠組だけを定義することが可能"
	itext t 1 "例: リストの要素のすべてに何かする"
higherOrder5 t = do
	setx t $ width / 3
	dvArrow t
	text t "他の言語の「構文」が普通の関数となる"

higherOrderChecks1 :: [Turtle -> IO ()]
higherOrderChecks1 = [
	higherOrderCheck1, higherOrderCheck2, higherOrderCheck3,
	higherOrderCheck4, higherOrderCheck5, higherOrderCheck6
 ]

higherOrderCheck1, higherOrderCheck2, higherOrderCheck3,
	higherOrderCheck4, higherOrderCheck5, higherOrderCheck6
	:: Turtle -> IO ()
higherOrderCheck1 t = do
	writeTopTitle t "高階関数(練習問題)"
	semititle t "以下の関数を定義せよ"
higherOrderCheck2 t = text t "与えられた関数を3回適用する関数"
higherOrderCheck3 t = do
	text t "10を底とした対数を求める関数を返す関数"
	itext t 1 "(ちなみに、logBase 10 1000 => 3)"
	text t ""
higherOrderCheck4 t = text t "答え:"
higherOrderCheck5 t = do
	text t "与えられた関数を3回適用する関数"
	itext t 1 "threeTimes fun x = fun (fun (fun x))"
higherOrderCheck6 t = do
	text t "10を底とした対数を求める関数を返す関数"
	itext t 1 "log10 = \\x -> logBase 10 x"

transparencies1 :: [Turtle -> IO ()]
transparencies1 = [
	transparency1, transparency2, transparency3, transparency4, transparency5
 ]

transparency1, transparency2, transparency3, transparency4, transparency5
	:: Turtle -> IO ()
transparency1 t = writeTopTitle t "参照透過性とは?"
transparency2 t = do
	text t "同じ関数を同じ入力で呼び出せば"
	itext t 1 "出力は常に同じであるという性質"
	text t ""
transparency3 t = do
	text t "参照透過ではない例"
transparency4 t = do
	itext t 1 "C 言語"
	itext t 1 "counter() => 0"
	itext t 1 "counter() => 1"
	itext t 1 "counter() => 2"
	itext t 1 ""
transparency5 t = do
	itext t 1 "Ruby"
	itext t 1 "counter.count => 0"
	itext t 1 "counter.count => 1"
	itext t 1 "counter.count => 2"

transparencies2 :: [Turtle -> IO ()]
transparencies2 = [transparency6, transparency7, transparency8]

transparency6, transparency7, transparency8 :: Turtle -> IO ()
transparency6 t = do
	writeTopTitle t "参照透過性とは?"
	text t "Haskellでは同じ入力からは常に同じ出力"
	setx t $ width / 3
	dvArrow t
	text t "関数適用はその出力である値に置き換えることができる"
	itext t 1 $ "f x => 3"
	itext t 1 $ "g (f x) == g 3"
	text t "この場合、f x と 3 は全く同じ物と考えてよい"
	text t ""
transparency7 t = do
	semititle t "Haskellでの「関数」とは"
	text t "動作や手続き?"
transparency8 t = do
	let v = normalF * 9 / 8
	setheading t 90
	forward t v
	setx t $ width / 8
	y <- ycor t
	pensize t 2
	pendown t
	goto t (width / 3) (y + v)
	penup t
	forward t v
	pendown t
	goto t (width / 8) (y + v)
	penup t
	pensize t 1
	text t "「置き換え規則」である"

whatIsTypes1 :: [Turtle -> IO ()]
whatIsTypes1 = [
	whatIsType1, whatIsType2, whatIsType3, whatIsType3_1, whatIsType4,
	whatIsType4_5, whatIsType5, whatIsType6, whatIsType7
 ]

whatIsType1, whatIsType2, whatIsType3, whatIsType3_1,
	whatIsType4, whatIsType4_5,
	whatIsType5, whatIsType6, whatIsType7 :: Turtle -> IO ()
whatIsType1 t = writeTopTitle t "型とは?"
whatIsType2 t = semititle t "値の集合"
whatIsType3 t = do
	text t "Int: 1, 2, 3, ..."
	text t "Char: 'a', 'b', 'c' ..."
whatIsType3_1 t = do
	text t "* 値valが型Typeに属するとき「valはType型の値」という"
	text t ""
whatIsType4 t = semititle t "再び... 関数とは?"
whatIsType4_5 t = do
	text t "ある集合に属する値を他の集合に属する値に写像するもの"
	itext t 3 "(写像: 入力から出力を得ること)"
whatIsType5 t = text t "例:"
whatIsType6 t = text t "絶対値 => 数の集合から数の集合への写像"
whatIsType7 t = text t "文字コードを返す関数 => 文字の集合から数の集合への写像"

whatIsTypes2 :: [Turtle -> IO ()]
whatIsTypes2 = [whatIsType8, whatIsType9, whatIsType10]

whatIsType8, whatIsType9, whatIsType10 :: Turtle -> IO ()
whatIsType8 t = do
	writeTopTitle t "型とは?"
	semititle t "関数の型"
whatIsType9 t = do
	text t "関数にも型がある"
	text t "Type1 の値を取り、Type2 の値を返す関数の型を"
	text t "Haskell では Type1 -> Type2 と表記する"
	text t ""
whatIsType10 t = do
	semititle t "型の宣言"
	text t "Haskell では var :: Type のような形で型を宣言する"

whatIsTypeChecks1 :: [Turtle -> IO ()]
whatIsTypeChecks1 = [
	whatIsTypeCheck1, whatIsTypeCheck2, whatIsTypeCheck3,
	whatIsTypeCheck4, whatIsTypeCheck5, whatIsTypeCheck6
 ]

whatIsTypeCheck1, whatIsTypeCheck2, whatIsTypeCheck3,
	whatIsTypeCheck4, whatIsTypeCheck5, whatIsTypeCheck6
	:: Turtle -> IO ()
whatIsTypeCheck1 t = do
	writeTopTitle t "型とは?(練習問題)"
	text t "以下の関数の型を Haskell で宣言せよ"
	text t ""
whatIsTypeCheck2 t = text t "Int 型の値を取りその絶対値を返す関数 abs"
whatIsTypeCheck3 t = do
	text t "Char 型の値を取り文字コード(Int)を返す関数 ord"
	text t ""
whatIsTypeCheck4 t = text t "答え"
whatIsTypeCheck5 t = text t "絶対値を返す関数: abs :: Int -> Int"
whatIsTypeCheck6 t = text t "文字コードを返す関数: ord :: Char -> Int"

staticTypings1 :: [Turtle -> IO ()]
staticTypings1 = [
	staticTyping1, staticTyping2, staticTyping3, staticTyping4,
	staticTyping5, staticTyping6, staticTyping7, staticTyping8
 ]

staticTyping1, staticTyping2, staticTyping3, staticTyping4,
	staticTyping5, staticTyping6, staticTyping7, staticTyping8
	:: Turtle -> IO ()
staticTyping1 t = do
	writeTopTitle t "静的型付けとは?"
staticTyping2 t = semititle t "動的型付けとは?"
staticTyping3 t = do
	text t "* 関数は定義域と値域を持たない"
	itext t 1 "(定義域: 入力の範囲 値域: 出力の範囲)"
staticTyping4 t = text t "* 関数はあらゆる型の値を入力される可能性がある"
staticTyping5 t = text t "* 関数はあらゆる型の値を出力する可能性がある"
staticTyping6 t = do
	setx t $ width / 3
	dvArrowL t 12
	text t "あらゆる値に対して動作を保証する必要がある"
staticTyping7 t = do
	semititle t "静的型付けの場合"
	text t "* 決められた範囲の値についてだけ定義すれば良い"
staticTyping8 t = do
	setx t $ width / 3
	dvArrowL t 12
	itext t 2 "楽ちん"

typeFlexibilities1 :: [Turtle -> IO ()]
typeFlexibilities1 = [
	typeFlexibility1, typeFlexibility2, typeFlexibility3, typeFlexibility4,
	typeFlexibility5, typeFlexibility6
 ]

typeFlexibility1, typeFlexibility2, typeFlexibility3, typeFlexibility4,
	typeFlexibility5, typeFlexibility6
	:: Turtle -> IO ()
typeFlexibility1 t = writeTopTitle t "型の柔軟性"
typeFlexibility2 t = text t "Haskellでは柔軟な型を持つ関数は作れないの?"
typeFlexibility3 t = do
	setx t $ width / 6
	setheading t $ -90
	forward t $ normalF * 13 / 8
	left t 90
	arrow t $ width / 35
	left t 90
	forward t $ normalF * 13 / 8
	itext t 1 "柔軟性の範囲を正確に決めておけば良い"
	text t ""
typeFlexibility4 t = do
	text t "例: 関数idはすべての型の値を取り同じ型の値を返す関数"
typeFlexibility5 t = do
	itext t 1 "id は「正確に」すべての型の値が取れる"
	text t ""
typeFlexibility6 t = do
	text t "言うなれば"
	text t ""
	semititle t "「厳密に定義された曖昧さ」"
	text t ""
	itext t 4 "ということ"

lazyEvaluations1 :: [Turtle -> IO ()]
lazyEvaluations1 = [
	lazyEvaluation1, lazyEvaluation2, lazyEvaluation2_1, lazyEvaluation2_2,
	lazyEvaluation3, lazyEvaluation4, lazyEvaluation5, lazyEvaluation6,
	lazyEvaluation7
 ]

lazyEvaluation1, lazyEvaluation2, lazyEvaluation2_1, lazyEvaluation2_2,
	lazyEvaluation3, lazyEvaluation4,
	lazyEvaluation5, lazyEvaluation6, lazyEvaluation7
	:: Turtle -> IO ()
lazyEvaluation1 t = writeTopTitle t "遅延性とは?"
lazyEvaluation2 t = do
	text t "使わない構造は展開されないということ"
	text t ""
lazyEvaluation2_1 t = do
	text t "* 引数部分に無限ループがあったとしても"
	itext t 1 "その引数が使われていなければ問題ない"
lazyEvaluation2_2 t = do
	text t "* 無限リストを使うこともできる"
	text t ""
lazyEvaluation3 t = do
	text t "例1: myIf 関数"
lazyEvaluation4 t = do
	setheading t 90
	forward t $ normalF * 18 / 8
	setx t $ width * 51 / 144
	rightArrow t
	itext t 3 "先行評価だとthen部もelse部も先に評価"
lazyEvaluation5 t = do
	itext t 1 "遅延評価なら問題ない"
	text t ""
lazyEvaluation6 t = do
	text t "例2: 無限リスト"
lazyEvaluation7 t = do
	itext t 1 "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)"
	itext t 1 "print $ take 100 fibs"

lazyEvaluations2 :: [Turtle -> IO ()]
lazyEvaluations2 = [
	lazyEvaluation8, lazyEvaluation9, lazyEvaluation10, lazyEvaluation11,
	lazyEvaluation12
 ]

lazyEvaluation8, lazyEvaluation9, lazyEvaluation10, lazyEvaluation11,
	lazyEvaluation12 :: Turtle -> IO ()
lazyEvaluation8 t = do
	writeTopTitle t "遅延性とは?"
	text t "遅延性という言葉は使われていない"
	setx t $ width / 7
	rightArrow t
	itext t 1 "このスライド用に造語"
	text t ""
lazyEvaluation9 t = do
	text t "どうしてそんなことを?"
lazyEvaluation10 t = do
	text t "* 遅延評価と遅延型をまとめて表現したかった"
lazyEvaluation11 t = do
	text t "* 遅延型も造語"
	itext t 1 "遅延リストは普遍的な遅延するデータ構造のひとつ"
	itext t 1 "Haskell ではデータ構造の評価は遅延する"
lazyEvaluation12 t = do
	text t "* 正確に言うと"
	itext t 1 "「遅延評価と弱頭部正規形までの簡約」となるだろう"

lazyEvaluationChecks1 :: [Turtle -> IO ()]
lazyEvaluationChecks1 = [
	lazyEvaluationCheck1, lazyEvaluationCheck2, lazyEvaluationCheck3,
	lazyEvaluationCheck4, lazyEvaluationCheck5, lazyEvaluationCheck6
 ]

lazyEvaluationCheck1, lazyEvaluationCheck2, lazyEvaluationCheck3,
	lazyEvaluationCheck4, lazyEvaluationCheck5, lazyEvaluationCheck6
	:: Turtle -> IO ()
lazyEvaluationCheck1 t =  do
	writeTopTitle t "遅延性とは?(練習問題)"
	text t "以下の例について先行性と遅延性の"
	itext t 1 "それぞれについてどうなるか答えよ"
	text t ""
lazyEvaluationCheck2 t =
	text t "x = x; const y z = yのときのconst 8 x"
lazyEvaluationCheck3 t = do
	text t "ones = 1 : onesのときのtake 10 ones"
	text t ""
lazyEvaluationCheck4 t = text t "答え:"
lazyEvaluationCheck5 t = do
	text t "const 8 x"
	itext t 1 "先行性: 無限ループにより値がかえらない"
	itext t 1 "遅延性: 値8がかえる"
lazyEvaluationCheck6 t = do
	text t "take 10 ones"
	itext t 1 "先行性: onesの完全な評価のため値がかえらない"
	itext t 1 "遅延性: 1が10個はいったリストがかえる"

summaries1 :: [Turtle -> IO ()]
summaries1 = [
	summary1, summary2, summary3, summary4, summary5, summary6, summary7
 ]

summary1, summary2, summary3, summary4, summary5, summary6, summary7
	:: Turtle -> IO ()
summary1 t = writeTopTitle t "まとめ" >> text t ""
summary2 t = text t "* 関数とは「置き換え規則」である"
summary3 t = text t "* 型は入力と出力の値の範囲を示す"
summary4 t = text t "* 高階関数によって枠組の抽象化が可能"
summary5 t = text t "* 参照透過性は上記「関数」の性質に本質的"
summary6 t = text t "* 静的型付けは値の範囲が決まるので楽"
summary7 t = text t "* 遅延評価によって問題を自然に切り分けられる"
