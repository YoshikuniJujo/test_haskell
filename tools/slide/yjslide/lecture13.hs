module Main where

import Lecture

subtitle :: String
subtitle = "第12回 テスト"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, typeCheck, typeCheck2,
	proof, proof2, proof3, proof4, proof5,
	quickCheck
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでは型システム自体が一種のテストである", \t -> do
	itext t 1 "- 適切な型の設計が重要", \t -> do
	text t "* 関数は単なる置き換えと考えられるので", \t -> do
	itext t 1 "- プログラムの「正しさ」をある程度「証明」できる", \t -> do
	itext t 1 "- 手作業での簡約が証明となる", \t -> do
	text t "* 数学的に扱えない部分についてはテストが必要となる", \t -> do
	itext t 1 "- 入力の型(つまり範囲)が決まっている", \t -> do
	arrowIText t 2 "ランダムな値を生成してテストすることが可能", \t -> do
	arrowIText t 2 "QuickCheck", \t -> do
	itext t 1 "- IOが絡むもの、境界条件などの決まった値のテスト", \t -> do
	arrowIText t 2 "HUnit"
 ]

typeCheck :: Page
typeCheck = [\t -> do
	writeTopTitle t "型によるチェック"
	text t "", \t -> do
	text t "* 型の不整合によるバグ", \t -> do
	itext t 1 "- 動的型付けの言語では起こり得る", \t -> do
	itext t 1 "- 静的型付けの言語ではコンパイル時にチェック", \t -> do
	text t "例: (python)"
	text t "x = input(\"string/number? \")"
	text t "if x == \"string\":"
	itext t 1 "y = \"Hello\""
	text t "else:"
	itext t 1 "y = 88"
	text t "print(y + \", world!\")"
 ]

typeCheck2 :: Page
typeCheck2 = [\t -> do
	writeTopTitle t "型によるチェック"
	text t "", \t -> do
	text t "* 型付けのやりかたによりチェックの強度は変化する", \t -> do
	text t "例:"
	text t "座標を(Double, Double)で表現", \t -> do
	arrowIText t 1 "極座標と直交座標間の型レベルでの区別ができない", \t -> do
	text t "Rec Double Double, Pol Double Doubleで表現", \t -> do
	arrowIText t 1 "適切な座標系であることを型レベルで保証できる", \t -> do
	dvArrowShort t
	text t "適切な型付けをすればテストは半分終わったようなもの"
 ]

proof :: Page
proof = [\t -> do
	writeTopTitle t "手作業での証明"
	text t "", \t -> do
	text t "length [] = 0"
	text t "length (_ : xs) = 1 + length xs"
	text t ""
	text t "map f [] = []"
	text t "map f (x : xs) = f x : map f xs"
	text t ""
	text t "length xs == length (map f xs)を証明してみる"
	text t ""
 ]

proof2 :: Page
proof2 = [\t -> do
	writeTopTitle t "手作業での証明"
	text t "", \t -> do
	text t "xsが空リストのとき"
	text t "", \t -> do
	text t "length xs -> length []"
	text t "length (map f xs) -> length (map f []) -> length []"
	text t ""
	text t "で成り立つ"
 ]

proof3 :: Page
proof3 = [\t -> do
	writeTopTitle t "手作業での証明"
	text t "", \t -> do
	text t "length k == length (map f k)が成り立つと仮定したときに"
	text t "length (x : k) == length (map f (x : k))を証明する"
	text t "", \t -> do
	text t "length (x : k) -> 1 + length k"
	text t "length (map f (x : k)) -> length (f x : map f k)"
	itext t 2 "-> 1 + length (map f k)"
	text t "", \t -> do
	text t "よって、length (x : k) == length (map f (x : k))"
 ]

proof4 :: Page
proof4 = [\t -> do
	writeTopTitle t "手作業での証明"
	text t "", \t -> do
	text t "* length xs == length (map f xs)を証明した", \t -> do
	text t "* xsが空リストのときに成り立つことを証明", \t -> do
	text t "* xsがkのときに成り立つならば"
	itext t 1 "xsが(x : k)のときに成り立つことを証明", \t -> do
	text t "* 数学的帰納法を使った", \t -> do
	dvArrowShort t
	text t "length (map f xs)は安全にlength xsと書き換え可"
 ]

proof5 :: Page
proof5 = [\t -> do
	writeTopTitle t "手作業での証明(まとめ)"
	text t "", \t -> do
	text t "* Haskellには正しいことを証明できる部分がある", \t -> do
	text t "* 多くの部分ではこのような証明は困難だが"
	itext t 1 "証明済みの部分に自信が持てるということは強力", \t -> do
	text t "* 「証明」は「すべての値」で正しいことを示す", \t -> do
	itext t 1 "- これはテストでは到達困難なレベルの自信", \t -> do
	text t "* もちろん証明にまちがいがある場合もある"
 ]

quickCheck :: Page
quickCheck = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* ランダムな値を生成してテストを実行する", \t -> do
	text t "* 抜き取り検査のようなもの", \t -> do
	text t "* 統計学的な正しさを保証する"
	text t "", \t -> do
	text t "* すべての値で必ず成り立つと期待される性質を記述する", \t -> do
	text t "* その性質は型 a -> Bool を持つ関数となる", \t -> do
	text t "* 慣習として prop_ をプレフィックスとする"
	text t "", \t -> do
	text t "prop_lengthMap xs = length xs == length (map (* 10) xs)"
	text t "> quickCheck prop_lengthMap"
	text t "+++ OK, passed 100 tests."
 ]
