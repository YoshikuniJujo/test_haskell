module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第2回 Haskellの特徴"

pages :: [Page]
pages = [
	titlePage,
	feature,
	pure 0, functions, functionChecks,
	pure 1, firstclasses1, firstclassExams1, syntaxes1,
		higherOrders1, higherOrders2, higherOrderChecks1,
	pure 2, transparencies1, transparencies2,
	pure 3, whatIsTypes1, whatIsTypes2, whatIsTypeChecks1,
		staticTypings1, typeFlexibilities1,
	pure 4, lazyEvaluations1, lazyEvaluations2, lazyEvaluationChecks1,
	summaries1]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

feature :: Page
feature = [\t -> do
	writeTopTitle t "Haskellの特徴"
	writeImageRight t haskellBCurry
	text t "純粋関数型言語であり"
	itext t 1 "* 第一級関数"
	itext t 1 "* 参照透過性"
	itext t 1 "* 静的型付け"
	itext t 1 "* 遅延性"
	text t "という特徴を持つ", \t -> do
	dvArrowShort t
	text t "概念の本質的な部分をそのまま表現できる"
	text t "", \t -> do
	text t "例: 小さい方から10個の素数が欲しい", \t -> do
	text t "=> すべての素数を求める", \t -> do
	text t "-> 小さい方から10個取り出す"]

pure :: Int -> Page
pure n = [\t -> do
	writeTopTitle t "Haskellの特徴"
	oneshot t $ do
		(if n == 0 then withRed t else id) $ semititle t "純粋関数型言語"
		(if n == 1 then withRed t else id) $ semititle t "* 第一級関数"
		(if n == 2 then withRed t else id) $ semititle t "* 参照透過性"
		(if n == 3 then withRed t else id) $ semititle t "* 静的型付け"
		(if n == 4 then withRed t else id) $ semititle t "* 遅延性"
	]

functions :: Page
functions = [\t -> do
	writeTopTitle t "関数とは?", \t -> do
	text t "0個以上の入力値をひとつの出力値へ変えるルール"
	graphArrowString t 10 50 (Just "入力1") Nothing
	graphArrowString t 10 70 (Just "入力2") Nothing
	drawRect t (13500 / 364) (200 / 5) 25 35
	graphArrowString t (130 / 2) (235 / 4) Nothing (Just "出力")]

functionChecks :: Page
functionChecks = [\t -> do
	writeTopTitle t "関数とは?(練習問題)"
	semititle t "以下の「関数」の入力と出力を述べよ", \t -> do
	text t "足し算", \t -> do
	text t "翻訳", \t -> do
	text t "与えられた文字列を表示する機能", \t -> do
	text t "" >> text t "答え", \t -> do
	text t "足し算: 2つの数 -> 数", \t -> do
	text t "翻訳: ある言語の文 -> 別の言語の文", \t -> do
	text t "与えられた文字列を表示する機能: 文字列 -> 動作"]

firstclasses1 :: Page
firstclasses1 = [\t -> do
	writeTopTitle t "第一級関数とは?", \t -> do
	text t "関数が第一級オブジェクトであるということ", \t -> do
	writeNextTitle t "第一級オブジェクトとは?", \t -> do
	text t "* リテラルとして表現できる"
	text t "* 変数に格納できる"
	text t "* データ構造に格納できる"
	text t "* 関数の引数になれる"
	text t "* 関数の返り値になれる"]

firstclassExams1 :: Page
firstclassExams1 = [\t -> do
	writeTopTitle t "第一級関数とは?"
	text t "* リテラルとして表現できる"
	itext t 1 "\\x -> x * x", \t -> do
	text t "* 変数に格納できる"
	itext t 1 "square = \\x -> x * x", \t -> do
	text t "* データ構造に格納できる"
	itext t 1 "[\\x -> x * x]", \t -> do
	text t "* 関数の引数になれる"
	itext t 1 "twice fun x = fun (fun x)"
	itext t 1 "twice sqrt 9 => 1.7320508075688772", \t -> do
	text t "* 関数の返り値になれる"
	itext t 1 "addN n = \\x -> x + n"
	itext t 1 "(addN 3) 8 => 11"]

syntaxes1 :: Page
syntaxes1 = [\t -> do
	writeTopTitle t "ここまでに出てきた構文", \t -> do
	text t "* 関数リテラル: \\parm -> expression", \t -> do
	text t "* リストリテラル: [expression1, expression2, ... ]", \t -> do
	text t "* 定義: var = expression", \t -> do
	text t "* 関数定義: fun parm1 parm2 = expression", \t -> do
	text t "* 関数適用: fun arg1 arg2"
	text t "", \t -> do
	text t "(注1) 変数の定義と0個の引数を取る関数の定義"
	itext t 1 "とは同じこと", \t -> do
	text t "(注2) 関数適用の結果を`=> value'のような"
	itext t 1 "形で示すが、これはHaskellの構文ではない。"]

higherOrders1 :: Page
higherOrders1 = [\t -> do
	writeTopTitle t "高階関数"
	text t "高階関数とは引数または返り値が関数であるような関数", \t -> do
	text t "つまり"
	text t ""
	text t "関数が第一級オブジェクトである"
	dvArrow t
	text t "高階関数が書ける"
	text t ""
	text t "ということ"]

higherOrders2 :: Page
higherOrders2 = [\t -> do
	writeTopTitle t "高階関数"
	oneshot t $
		text t "高階関数とは引数または返り値が関数であるような関数"
	text t ""
	text t "何がうれしいの?"
	text t "", \t -> do
	text t "* より高レベルな抽象化"
	itext t 1 "枠組だけを定義することが可能"
	itext t 1 "例: リストの要素のすべてに何かする", \t -> do
	dvArrow t
	text t "他の言語の「構文」が普通の関数となる"]

higherOrderChecks1 :: Page
higherOrderChecks1 = [\t -> do
	writeTopTitle t "高階関数(練習問題)"
	semititle t "以下の関数を定義せよ", \t -> do
	text t "与えられた関数を3回適用する関数", \t -> do
	text t "10を底とした対数を求める関数を返す関数"
	itext t 1 "(ちなみに、logBase 10 1000 => 3)"
	text t "", \t -> do
	text t "答え:", \t -> do
	text t "与えられた関数を3回適用する関数"
	itext t 1 "threeTimes fun x = fun (fun (fun x))", \t -> do
	text t "10を底とした対数を求める関数を返す関数"
	itext t 1 "log10 = \\x -> logBase 10 x"]

transparencies1 :: Page
transparencies1 = [\t -> do
	writeTopTitle t "参照透過性とは?", \t -> do
	text t "同じ関数を同じ入力で呼び出せば"
	itext t 1 "出力は常に同じであるという性質"
	text t "", \t -> do
	text t "参照透過ではない例", \t -> do
	itext t 1 "C 言語"
	itext t 1 "counter() => 0"
	itext t 1 "counter() => 1"
	itext t 1 "counter() => 2"
	itext t 1 "", \t -> do
	itext t 1 "Ruby"
	itext t 1 "counter.count => 0"
	itext t 1 "counter.count => 1"
	itext t 1 "counter.count => 2"]

transparencies2 :: Page
transparencies2 = [\t -> do
	writeTopTitle t "参照透過性とは?"
	text t "Haskellでは同じ入力からは常に同じ出力"
	dvArrow t
	text t "関数適用はその出力である値に置き換えることができる"
	itext t 1 $ "f x => 3"
	itext t 1 $ "g (f x) == g 3"
	text t "この場合、f x と 3 は全く同じ物と考えてよい"
	text t "", \t -> do
	semititle t "Haskellでの「関数」とは"
	text t "動作や手続き?", \t -> do
	xmark t "動作や手続き?"
	text t "「置き換え規則」である"]

whatIsTypes1 :: Page
whatIsTypes1 = [\t -> do
	writeTopTitle t "型とは?", \t -> do
	semititle t "値の集合", \t -> do
	text t "Int: 1, 2, 3, ..."
	text t "Char: 'a', 'b', 'c' ...", \t -> do
	text t "* 値valが型Typeに属するとき「valはType型の値」という"
	text t "", \t -> do
	semititle t "再び... 関数とは?", \t -> do
	text t "ある集合に属する値を他の集合に属する値に写像するもの"
	itext t 3 "(写像: 入力から出力を得ること)", \t -> do
	text t "例:", \t -> do
	text t "絶対値 => 数の集合から数の集合への写像", \t -> do
	text t "文字コードを返す関数 => 文字の集合から数の集合への写像"]

whatIsTypes2 :: Page
whatIsTypes2 = [\t -> do
	writeTopTitle t "型とは?"
	semititle t "関数の型", \t -> do
	text t "関数にも型がある"
	text t "Type1 の値を取り、Type2 の値を返す関数の型を"
	text t "Haskellでは Type1 -> Type2 と表記する"
	text t "", \t -> do
	semititle t "型の宣言"
	text t "Haskellでは var :: Type のような形で型を宣言する"]

whatIsTypeChecks1 :: Page
whatIsTypeChecks1 = [\t -> do
	writeTopTitle t "型とは?(練習問題)"
	text t "以下の関数の型をHaskellで宣言せよ"
	text t "", \t -> do
	text t "Int 型の値を取りその絶対値を返す関数 abs", \t -> do
	text t "Char 型の値を取り文字コード(Int)を返す関数 ord"
	text t "", \t -> do
	text t "答え", \t -> do
	text t "絶対値を返す関数: abs :: Int -> Int", \t -> do
	text t "文字コードを返す関数: ord :: Char -> Int"]

staticTypings1 :: Page
staticTypings1 = [\t -> do
	writeTopTitle t "静的型付けとは?", \t -> do
	semititle t "動的型付けとは?", \t -> do
	text t "* 関数は定義域と値域を持たない"
	itext t 1 "(定義域: 入力の範囲 値域: 出力の範囲)", \t -> do
	text t "* 関数はあらゆる型の値を入力される可能性がある", \t -> do
	text t "* 関数はあらゆる型の値を出力する可能性がある", \t -> do
	dvArrowShort t
	text t "あらゆる値に対して動作を保証する必要がある", \t -> do
	semititle t "静的型付けの場合"
	text t "* 決められた範囲の値についてだけ定義すれば良い", \t -> do
	dvArrowShort t
	itext t 2 "楽ちん"]

typeFlexibilities1 :: Page
typeFlexibilities1 = [\t -> do
	writeTopTitle t "型の柔軟性", \t -> do
	text t "Haskellでは柔軟な型を持つ関数は作れないの?", \t -> do
	arrowIText t 1 "柔軟性の範囲を正確に決めておけば良い"
	text t "", \t -> do
	text t "例: 関数idはすべての型の値を取り同じ型の値を返す関数", \t -> do
	itext t 1 "id は「正確に」すべての型の値が取れる"
	text t "", \t -> do
	text t "言うなれば"
	text t ""
	semititle t "「厳密に定義された曖昧さ」"
	text t ""
	itext t 4 "ということ"]

lazyEvaluations1 :: Page
lazyEvaluations1 = [\t -> do
	writeTopTitle t "遅延性とは?", \t -> do
	text t "使わない構造は展開されないということ"
	text t "", \t -> do
	text t "* 引数部分に無限ループがあったとしても"
	itext t 1 "その引数が使われていなければ問題ない", \t -> do
	text t "* 無限リストを使うこともできる"
	text t "", \t -> do
	text t "例1: myIf 関数", \t -> do
	preLine t
	arrowIText t 3 "先行評価だとthen部もelse部も先に評価", \t -> do
	itext t 1 "遅延評価なら問題ない"
	text t "", \t -> do
	text t "例2: 無限リスト", \t -> do
	itext t 1 "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)"
	itext t 1 "print $ take 100 fibs"]

lazyEvaluations2 :: Page
lazyEvaluations2 = [\t -> do
	writeTopTitle t "遅延性とは?"
	text t "遅延性という言葉は使われていない"
	arrowIText t 1 "このスライド用に造語"
	text t "", \t -> do
	text t "どうしてそんなことを?", \t -> do
	text t "* 遅延評価と遅延型をまとめて表現したかった", \t -> do
	text t "* 遅延型も造語"
	itext t 1 "遅延リストは普遍的な遅延するデータ構造のひとつ"
	itext t 1 "Haskellではデータ構造の評価は遅延する", \t -> do
	text t "* 正確に言うと"
	itext t 1 "「遅延評価と弱頭部正規形までの簡約」となるだろう"]

lazyEvaluationChecks1 :: Page
lazyEvaluationChecks1 = [\t -> do
	writeTopTitle t "遅延性とは?(練習問題)"
	text t "以下の例について先行性と遅延性の"
	itext t 1 "それぞれについてどうなるか答えよ"
	text t "", \t -> do
	text t "x = x; const y z = yのときのconst 8 x", \t -> do
	text t "ones = 1 : onesのときのtake 10 ones"
	text t "", \t -> do
	text t "答え:", \t -> do
	text t "const 8 x"
	itext t 1 "先行性: 無限ループにより値がかえらない"
	itext t 1 "遅延性: 値8がかえる", \t -> do
	text t "take 10 ones"
	itext t 1 "先行性: onesの完全な評価のため値がかえらない"
	itext t 1 "遅延性: 1が10個はいったリストがかえる"]

summaries1 :: Page
summaries1 = [\t ->
	writeTopTitle t "まとめ" >> text t "", \t ->
	text t "* 関数とは「置き換え規則」である", \t ->
	text t "* 型は入力と出力の値の範囲を示す", \t ->
	text t "* 高階関数によって枠組の抽象化が可能", \t ->
	text t "* 参照透過性は上記「関数」の性質に本質的", \t ->
	text t "* 静的型付けは値の範囲が決まるので楽", \t ->
	text t "* 遅延評価によって問題を自然に切り分けられる"]
