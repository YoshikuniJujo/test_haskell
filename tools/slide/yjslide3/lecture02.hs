import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第1回 型・リスト"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	{- aboutType, aboutType2, aboutType3, -} aboutType4, aboutType5,
	aboutType6, aboutType7, aboutType8, aboutType9, aboutType10,
	aboutType11, aboutType12, aboutType13, typeSummary,
	aboutIterate, aboutIterate2, aboutIterate3, aboutIterate4,
	aboutIterate5,
	iterateSpace
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型は関数を組み合わせる際に重要な役割を持つ", \t -> do
	text t "* プログラミングの機能として「くりかえし」は重要", \t -> do
	text t "* 「くりかえし」を実現するために", \t -> do
	itext t 1 "- 手続き型言語では状態変化を使う", \t -> do
	itext t 1 "- Haskellでは主にリストを使う", \t -> do
	text t "* 今回は関数・型・リストについて学ぼう"
 ]

aboutType :: Page
aboutType = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* 型とは「値の種類」である", \t -> do
	text t "* 3と8は同じ種類の値だが\"hello\"は違う種類の値となる", \t -> do
	text t "* 3と8は同じように扱えるが\"hello\"は同じようには扱えない", \t -> do
	text t "* 3や8には1を足すことができる", \t -> do
	text t "* \"hello\"や\"hi\"はさかさにすることができる", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> 3 + 1", \t -> do
	itext t 1 $ show $ 3 + 1, \t -> do
	itext t 1 "Prelude> reverse \"hello\"", \t -> do
	itext t 1 $ show $ reverse "hello"
 ]

aboutType2 :: Page
aboutType2 = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* \"hello\"や\"hi\"に1を足すことはできない", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "Prelude> \"hello\" + 1", \t -> do
	itext t 1 "..."
	itext t 1 "No instance for (Num [Char]) arising from ..."
	itext t 1 "...", \t -> do
	text t "* エラーの内容については今は理解できなくて良い", \t -> do
	itext t 1 "- Stringは[Char]の別名", \t -> do
	itext t 1 "- StringはNumじゃないということを言っている", \t -> do
	itext t 1 "- つまり文字列は数じゃないということ"
 ]

aboutType3 :: Page
aboutType3 = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* 3や8をさかさにすることはできない", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "Prelude> reverse 3", \t -> do
	itext t 1 "..."
	itext t 1 "No instance for (Num [a0]) arising from ..."
	itext t 1 "...", \t -> do
	text t "* エラーの内容については今は理解できなくて良い", \t -> do
	itext t 1 "- [a0]はNumじゃないと言っている", \t -> do
	itext t 1 "- リストは数じゃないということ"
 ]

aboutType4 :: Page
aboutType4 = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* 値には型があり、型によって「何ができるか」が決まる", \t -> do
	text t "* Haskellには文字を表すChar型が用意されている", \t -> do
	text t "* 文字はtoLowerで小文字にすることができる", \t -> do
	itext t 1 "Prelude> :m Data.Char", \t -> do
	itext t 2 "- toLowerを含むData.Charモジュールの読み込み", \t -> do
	itext t 1 "Prelude Data.Char> toLower 'A'", \t -> do
	itext t 1 "'a'"
 ]

aboutType5 :: Page
aboutType5 = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* Haskellには真偽を表現するBool型がある", \t -> do
	itext t 1 "- True, Falseの2つの値のみを持つ", \t -> do
	text t "* Trueを小文字にすることはできない", \t -> do
	itext t 1 "Prelude Data.Char> toLower True", \t -> do
	itext t 1 "..."
	itext t 1 "Couldn't match expected type `Char' with"
	itext t 1 "actual type `Bool'"
	itext t 1 "In the first argument of `toLower', namely `True'"
	itext t 1 "...", \t -> do
	text t "* 「TrueはBoolであってCharじゃないよ」と言われた"
 ]

aboutType6 :: Page
aboutType6 = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* 関数にも型がある", \t -> do
	text t "* 関数の型は入力の型と出力の型によって決まる", \t -> do
	text t "* たとえばtoLowerの型は文字を取って文字を返す関数で", \t -> do
	itext t 1 "型はChar -> Charとなる", \t -> do
	text t "* それでは、前の例の関数bmiの型はどうなるだろうか", \t -> do
	text t "* 入力と出力を倍精度浮動小数点数とする", \t -> do
	text t "* 倍精度浮動小数点数を表す型はDoubleなので", \t -> do
	text t "* bmiはDoubleを2こ取ってDoubleを返す関数となる、よって", \t -> do
	itext t 1 "Double -> Double -> Double"
 ]

aboutType7 :: Page
aboutType7 = [\t -> do
	writeTopTitle t "型"
	text t "", \t -> do
	text t "* 関数の型は以下のように表される", \t -> do
	itext t 1 "[引数1の型] -> [引数2の型] -> ... -> [返り値の型]", \t -> do
	text t "* この記法の合理性については後々明らかになる"
 ]

aboutType8 :: Page
aboutType8 = [\t -> do
	writeTopTitle t "型の重要性"
	text t "", \t -> do
	text t "* 関数を考えるときにはまず型を考える", \t -> do
	text t "* 関数の中身を作る前に以下を考える", \t -> do
	itext t 1 "- 出力としてほしいのはどういう種類の値か", \t -> do
	itext t 1 "- 関数の入力には何が必要か", \t -> do
	text t "* bmi関数の例では", \t -> do
	itext t 1 "- ほしいのはBMIを表す浮動小数点数", \t -> do
	itext t 1 "- 入力は身長と体重を表す2つの浮動小数点数", \t -> do
	text t "* ここでBMI値によって肥満かどうかを判定する関数を考える", \t -> do
	itext t 1 "- ほしいのは肥満かどうかを表すBool値", \t -> do
	itext t 1 "- 入力は同上の2つの浮動小数点数"
 ]

aboutType9 :: Page
aboutType9 = [\t -> do
	writeTopTitle t "型の宣言"
	text t "", \t -> do
	text t "* bmi関数を定義したときには型を宣言しなかった", \t -> do
	text t "* 型を宣言しない場合、型推論によって自動的に決まる", \t -> do
	text t "* しかし、型は宣言したほうが良い", \t -> do
	itext t 1 "- 自分がどういう関数を作っているのかを明確にする", \t -> do
	itext t 1 "- バグの多くは型チェックによって検出可能", \t -> do
	itext t 1 "- 型宣言は優れたドキュメントとなる", \t -> do
	itext t 1 "- こみいった型だと型推論が利かない場合がある", \t -> do
	text t "* 型宣言は以下のような記法となる", \t -> do
	itext t 1 "[関数名] :: [型]"
 ]

aboutType10 :: Page
aboutType10 = [\t -> do
	writeTopTitle t "型駆動開発"
	text t "", \t -> do
	text t "* 「型駆動開発」とは今思いついた言葉", \t -> do
	text t "* まずは型を決めてそれから中身をうめていくやりかた", \t -> do
	text t "* lecture/lecture01/bmi2.hsを作ってみよう", \t -> do
	itext t 1 "bmi :: Double -> Double -> Double"
	itext t 1 "bmi = undefined"
	itext t 1 ""
	itext t 1 "isObese :: Double -> Double -> Bool"
	itext t 1 "isObese = undefined", \t -> do
	text t "* undefinedは評価されるとエラーとなる値", \t -> do
	text t "* 今回はスタブとして使用した"
 ]

aboutType11 :: Page
aboutType11 = [\t -> do
	writeTopTitle t "型駆動開発"
	text t "", \t -> do
	itext t 0 "% ghci bmi2", \t -> do
	itext t 0 "*Main> bmi 218 160", \t -> do
	itext t 0 "*** Exception: Prelude.undefined", \t -> do
	itext t 0 "*Main> isObese 218 160", \t -> do
	itext t 0 "*** Exception: Prelude.undefined", \t -> do
	itext t 1 "- 当然エラーとなる", \t -> do
	itext t 0 "*Main> :type bmi", \t -> do
	itext t 0 "bmi :: Double -> Double -> Double", \t -> do
	itext t 0 "*Main> :type isObese", \t -> do
	itext t 0 "isObese :: Double -> Double -> Bool", \t -> do
	itext t 1 "- 対話環境では:typeで関数や値の型を表示できる"
 ]

aboutType12 :: Page
aboutType12 = [\t -> do
	writeTopTitle t "型駆動開発"
	text t "", \t -> do
	text t "* 開発のしかたにはボトムアップとトップダウンがある", \t -> do
	text t "* 今回の場合", \t -> do
	itext t 1 "- bmi関数を先に定義すればボトムアップとなり", \t -> do
	itext t 1 "- isObese関数を先に定義すればトップダウンとなる", \t -> do
	text t "* トップダウンで開発してみよう", \t -> do
	text t "* BMIが30以上で肥満とされるので", \t -> do
	itext t 1 "isObese h w = bmi h w >= 30", \t -> do
	text t "* ここでの思考は以下のようになる", \t -> do
	itext t 1 "- 身長と体重からBMIを求める関数bmiがあると仮定", \t -> do
	itext t 1 "- するとisObese h wはbmi h w >= 30で定義できる"
 ]

aboutType13 :: Page
aboutType13 = [\t -> do
	writeTopTitle t "型駆動開発"
	text t "", \t -> do
	text t "* bmi関数が定義されているという仮定のもとで", \t -> do
	itext t 1 "isObese関数が定義された", \t -> do
	text t "* よってここでbmi関数を定義すれば仕事は終わる", \t -> do
	itext t 1 "bmi h w = w / (h / 100) ^ 2", \t -> do
	text t "* 動かしてみよう", \t -> do
	itext t 1 "% ghci bmi2.hs", \t -> do
	itext t 1 "*Main> isObese 218 160", \t -> do
	itext t 1 "True", \t -> do
	text t "* チェ・ホンマンは肥満であると判定された", \t -> do
	itext t 1 "- BMIは高身長者には厳しい", \t -> do
	itext t 1 "- 脂肪と筋肉を区別できない"
 ]

typeSummary :: Page
typeSummary = [\t -> do
	writeTopTitle t "型(まとめ)"
	text t "", \t -> do
	text t "* 型とは値の種類", \t -> do
	text t "* 型が同じなら値によらずできることは同じ", \t -> do
	text t "* 関数を作るときにはまず型を考える", \t -> do
	itext t 1 "- 型が決まれば仕事は半分終わったようなもの", \t -> do
	itext t 1 "- 型チェックによってバグの多くは検出可", \t -> do
	itext t 1 "- 型はドキュメントとしても優れる", \t -> do
	text t "* 型の宣言は[関数内] :: [型]という形", \t -> do
	text t "* 型駆動開発をしてみた"
 ]

aboutIterate :: Page
aboutIterate = [\t -> do
	writeTopTitle t "くりかえし"
	text t "", \t -> do
	text t "* 0からnまでの二乗の和を求めるとする", \t -> do
	text t "* 手続き型言語ではこう考える", \t -> do
	itext t 1 "1. iに0を代入、sに0を代入", \t -> do
	itext t 1 "2. iがn以下のあいだ3, 4をくりかえす", \t -> do
	itext t 1 "3. sにiの二乗を足す", \t -> do
	itext t 1 "4. iに1を足す", \t -> do
	itext t 1 "5. sを返す", \t -> do
	text t "* リストを使うと以下のようになる", \t -> do
	itext t 1 "1. 0からnまでの数のリストを作成する", \t -> do
	itext t 1 "2. リストの値のすべてを二乗したリストを作る", \t -> do
	itext t 1 "3. リストの値の総和を求める"
 ]

aboutIterate2 :: Page
aboutIterate2 = [\t -> do
	writeTopTitle t "くりかえし"
	text t "", \t -> do
	text t "* 手続き型のほうは", \t -> do
	itext t 1 "- 本当に正しいのかが自明ではない", \t -> do
	itext t 1 "- 機械の気持ちになって手続きを追うことが必要", \t -> do
	itext t 1 "- iがn以下のとき?それともiがn+1以下のとき?等々", \t -> do
	text t "* リストを使ったやりかたは", \t -> do
	itext t 1 "- 手続きではなく構造を記述している", \t -> do
	itext t 1 "- アルゴリズムが正しいことは自明"
 ]

aboutIterate3 :: Page
aboutIterate3 = [\t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 「リストの値のすべてを二乗したリストを作る」を考える", \t -> do
	text t "* 「リストの値のすべてに何かする」という構造が抽出できる", \t -> do
	text t "* 多くの言語でこのような「構造」は「構文」として特別扱い", \t -> do
	itext t 1 "- C言語のfor文等", \t -> do
	text t "* 引数として関数を取る関数を作れる言語では", \t -> do
	itext t 1 "「構造」を関数として定義することが可能", \t -> do
	text t "* 引数として関数を取る関数は高階関数と呼ばれる", \t -> do
	text t "* 「リストの値のすべてに何かする」関数は用意されている", \t -> do
	itext t 1 "- map f lstでlstのすべての要素にfを適用する"
 ]

square :: Int -> Int
square = (^ 2)

ai4number1 :: Int
ai4number1 = unsafePerformIO $ randomRIO (2, 10)

aboutIterate4 :: Page
aboutIterate4 = [\t -> do
	writeTopTitle t "mapを使う"
	text t "", \t -> do
	text t "* 実際にmapを使ってみよう", \t -> do
	text t "* lecture/lecture01ディレクトリに移動して", \t -> do
	text t "* squareSum.hsファイルを作ろう", \t -> do
	text t "* まずはsquare関数を書く", \t -> do
	itext t 1 "square :: Int -> Int"
	itext t 2 "- Intは処理系依存の大きさの整数型", \t -> do
	itext t 1 "square x = x ^ 2", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "% ghci squareSum.hs", \t -> do
	itext t 1 $ "*Main> square " ++ show ai4number1, \t -> do
	itext t 1 $ show $ square ai4number1
 ]

aboutIterate5 :: Page
aboutIterate5 = [\t -> do
	writeTopTitle t "mapを使う"
	text t "", \t -> do
	text t "* 作った関数をghciで確認しながら開発を進めるやりかた", \t -> do
	itext t 1 "- よりシステマティックなquickcheckライブラリもある", \t -> do
	text t "* 関数がだいたい正しいことを確認しながら開発できる", \t -> do
	text t "* square関数を利用して目的の関数を作成する", \t -> do
	itext t 1 "squareAll :: [Int] -> [Int]", \t -> do
	itext t 2 "- 型Xのリストは[X]と書くことができる", \t -> do
	itext t 1 "squareAll ns = map square ns"
 ]

iterateSpace :: Page
iterateSpace = [\t -> do
	writeTopTitle t "空間効率"
	text t "", \t -> do
	text t "* 経験をつんだプログラマならば気になる点がある", \t -> do
	itext t 1 "- いちいちリストを作ると空間効率がひどいことになる", \t -> do
	text t "* それはたしかに正しい", \t -> do
	text t "* しかし、Haskellのリストは遅延リストなので", \t -> do
	itext t 1 "- 必要になるまでリストの要素は作られない", \t -> do
	itext t 1 "- 計算の過程で必要な要素は常にひとつ", \t -> do
	itext t 1 "- また、2度と使われない要素はGCされる", \t -> do
	arrowIText t 1 "空間効率の低下はない", \t -> do
	text t "* 関数のあいだでリストを渡していく形でプログラムを組める", \t -> do
	text t "* 実際には「くりかえし」と同じような動作となる"
 ]

aboutIterateX :: Page
aboutIterateX = [\t -> do
	text t "* 0からnまでの数のリストを作る", \t -> do
	itext t 1 "[0 .. n]"
 ]
