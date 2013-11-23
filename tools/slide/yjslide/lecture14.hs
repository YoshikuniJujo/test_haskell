module Main where

import Lecture

subtitle :: String
subtitle = "第14回 テスト"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, typeCheck, typeCheck2,
	proof, proof2, proof3, proof4, proof5,
	quickCheck, quickCheckFalse, quickCheckInteger,
	quickCheckMul, quickCheckList, quickCheckTypes, quickCheckAll,
	quickCheckSummary,
	hunitPrelude, hunitNotIO, hunitMessage, hunitIO, hunitSummary,
	summary
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
	text t "* 多くの部分ではこのような証明は困難", \t -> do
	text t "* しかし、証明済みの部分に自信が持てることは強力", \t -> do
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
	text t "* prop_ をプレフィックスとする"
	text t "", \t -> do
	text t "prop_lengthMap xs = length xs == length (map (* 8) xs)", \t -> do
	text t "> quickCheck prop_lengthMap"
	text t "+++ OK, passed 100 tests."
 ]

quickCheckFalse :: Page
quickCheckFalse = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* 誤った値があった場合", \t -> do
	itext t 1 "- できるだけ単純な結果を探してくれる", \t -> do
	text t "prop_lessThan288 :: Int -> Bool"
	text t "prop_lessThan288 = (< 288)", \t -> do
	text t "> quickCheck prop_lessThan288"
	text t "*** Failed! Falsifiable (after 31 tests and 9 shrinks):"
	text t "288", \t -> do
	itext t 1 "- 値の範囲をせばめできるだけ小さな値を求めた", \t -> do
	itext t 1 "- 範囲を9回せばめたことを示す", \t -> do
	itext t 1 "- できるだけ「小さい」失敗する値として288を返した"
 ]

quickCheckInteger :: Page
quickCheckInteger = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* Int等のBoundedクラスのインスタンス", \t -> do
	arrowIText t 1 "全範囲の値をチェックする", \t -> do
	text t "* Integerをチェックする際には注意が必要", \t -> do
	itext t 1 "- 指定された範囲の値だけチェック", \t -> do
	itext t 1 "- デフォルトでは-100から100まで", \t -> do
	itext t 1 "- 明示的に変更する必要がある", \t -> do
	text t "例:"
	text t "> quickCheckWith (stdArgs { maxSize = 1000 }) (< 100)"
	text t "*** Failed! Falsifiable (after 15 tests and 3 shrinks):"
	text t "100"
 ]

quickCheckMul :: Page
quickCheckMul = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* 複数の引数を持つ関数もチェック可能"
	text t "", \t -> do
	text t "例:"
	text t "prop_exp2 :: Integer -> Integer -> Bool"
	text t "prop_exp2 x y = x `shiftL` fromInteger y =="
	itext t 1 "floor (fromInteger x * 2 ** fromInteger y)", \t -> do
	text t "> quickCheck prop_exp2"
	text t "+++ OK, passed 100 tests."
 ]

quickCheckList :: Page
quickCheckList = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* リストやタプルを引数に取る関数もチェック可能"
	text t "", \t -> do
	text t "例:"
	text t "prop_sorted :: [Int] -> Bool"
	text t "prop_sorted = isSorted . sort", \t -> do
	text t "> quickCheck prop_sorted"
	text t "+++ OK, passed 100 tests."
 ]

quickCheckTypes :: Page
quickCheckTypes = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* 自分で作った型を引数として取る関数もチェック可能", \t -> do
	text t "* Arbitraryクラスのインスタンスとすれば良い", \t -> do
	text t "* 以下の関数を定義する", \t -> do
	itext t 1 "arbitrary :: Gen a"
	itext t 1 "shrink :: a -> [a]", \t -> do
	text t "* 自分で作った型をRandomクラスのインスタンスにする", \t -> do
	text t "* arbitraryは以下のように定義しておけば良い", \t -> do
	itext t 1 "arbitrary = choose (min, max)", \t -> do
	text t "* shrinkは失敗したときに値の範囲を狭めていくための関数", \t -> do
	itext t 1 "- その値が失敗したときに試す値のリストを返す"
 ]

quickCheckAll :: Page
quickCheckAll = [\t -> do
	writeTopTitle t "QuickCheck"
	text t "", \t -> do
	text t "* テストをghciからではなくrunghcで行いたい", \t -> do
	text t "* main = do"
	itext t 1 "prop_foo"
	itext t 1 "prop_bar"
	itext t 1 "..."
	itext t 1 "とすれば良い", \t -> do
	text t "* ボイラープレートが嫌いな人のために"
	itext t 1 "main = $quickCheckAll"
	itext t 1 "とすることができる"
 ]

quickCheckSummary :: Page
quickCheckSummary = [\t -> do
	writeTopTitle t "QuickCheck(まとめ)"
	text t "", \t -> do
	text t "* ランダムな値を使ってテストすることができる", \t -> do
	text t "* Bool値を返す関数を作り、名前をprop_fooとする", \t -> do
	text t "* main = $quickCheckAllとすればrunghcでテストできる", \t -> do
	text t "* 自分で作った型をテストに使いたい", \t -> do
	itext t 1 "- Randomクラスのインスタンスにする", \t -> do
	itext t 1 "- Arbitraryクラスのインスタンスにする", \t -> do
	itext t 1 "- arbitrary = choose (min, max)とする", \t -> do
	itext t 1 "- shrinkは定義しなくても良い", \t -> do
	text t "* Integerを使うときは注意が必要", \t -> do
	itext t 1 "- デフォルトでは-100から100の範囲", \t -> do
	itext t 1 "- quickCheckWith (stdArgs{ maxSize = 1000 }) ..."
 ]

hunitPrelude :: Page
hunitPrelude = [\t -> do
	writeTopTitle t "HUnit"
	text t "", \t -> do
	text t "* 決まった値でテストを行いたいとき", \t -> do
	itext t 1 "- 境界条件をテストするときなど", \t -> do
	text t "* IOが絡む関数をテストするとき", \t -> do
	dvArrowShort t
	itext t 1 "HUnitが有用"
 ]

hunitNotIO :: Page
hunitNotIO = [\t -> do
	writeTopTitle t "HUnit"
	text t "", \t -> do
	text t "* main = runTestTT $ \"test name\" ~: [ ... ]", \t -> do
	text t "* ... の部分に複数のテストを書けば良い", \t -> do
	text t "* ~?, ~=?, ~?=の三種の演算子が用意されている", \t -> do
	itext t 1 "- [テスト] ~? [メッセージ]", \t -> do
	itext t 1 "- [期待する値] ~=? [チェックする値]", \t -> do
	itext t 1 "- [チェックする値] ~?= [期待する値]", \t -> do
	text t "例:"
	itext t 1 "isAscii 'あ' ~? \"'あ' is not ASCII\"", \t -> do
	itext t 1 "9 ~=? 3 + 5", \t -> do
	itext t 1 "3 + 5 ~?= 9"
 ]

hunitMessage :: Page
hunitMessage = [\t -> do
	writeTopTitle t "HUnit"
	text t "", \t -> do
	text t "* テストにはタイトルをつけることができる", \t -> do
	text t "* ~:演算子を使う", \t -> do
	itext t 1 "- [タイトル] ~: [テスト]", \t -> do
	text t "例:"
	itext t 1 "\"'3 + 5' is expected to be 9\" ~: 9 ~=? 3 + 5"
	text t "", \t -> do
	text t "### Failure in: test name:1:'3 + 5' is expected to be 9"
	text t "expected: 9"
	text t " but got: 8"
	text t "Cases: 1 Tried: 1 Errors: 0 Failures: 1"
 ]

hunitIO :: Page
hunitIO = [\t -> do
	writeTopTitle t "HUnit"
	text t "", \t -> do
	text t "* IOの関わるテスト", \t -> do
	itext t 1 "- 最終結果のみではなく途中経過もチェックしたい", \t -> do
	itext t 1 "- 前提条件や事後条件もチェックしたい", \t -> do
	text t "* @?, @=?, @?=の3つの関数が用意されている", \t -> do
	itext t 1 "- ~?, ~=?, ~?=と似ている", \t -> do
	itext t 1 "- IOのなかで使うことができる", \t -> do
	text t "例:"
	text t "do"
	preLine t
	itext t 1 "doesFileExist file @? \"File is not exist.\""
	itext t 1 "cnt <- readFile file"
	itext t 1 "\"foo\\n\" @=? cnt"
 ]

hunitSummary :: Page
hunitSummary = [\t -> do
	writeTopTitle t "HUnit"
	text t "", \t -> do
	text t "* HUnitの使いかたを学んだ", \t -> do
	text t "* main = runTestTT $ \"test title\" ~: [ ... ]", \t -> do
	text t "* IO以外では~?, ~=?, ~?=を使う", \t -> do
	text t "* IO内では@?, @=?, @?=が使える", \t -> do
	text t "* ~:でテストのタイトルをつけることができる"

 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellでのテストについて学んだ", \t -> do
	text t "* 型の設計をきちんとする", \t -> do
	itext t 1 "- 型チェック自体がテストになる", \t -> do
	text t "* 手作業での証明", \t -> do
	itext t 1 "- 数学的な証明が可能な部分がある", \t -> do
	text t "* 満たすべき性質を列挙しランダム値によるテスト", \t -> do
	arrowIText t 1 "QuickCheck", \t -> do
	text t "* 用意したテストを実行する", \t -> do
	arrowIText t 1 "HUnit"
 ]
