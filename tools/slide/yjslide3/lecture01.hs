import Lecture

subtitle :: String
subtitle = "第1回 関数・型・リスト"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, function, apply,
	defineFun, defineFun2, defineFun3, funSummary,
	{- aboutType, aboutType2, aboutType3, -} aboutType4, aboutType5,
	aboutType6, aboutType7, aboutType8, aboutType9, aboutType10,
	aboutType11, aboutType12, aboutType13, typeSummary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでプログラミングをするということは", \t -> do
	itext t 1 "関数を様々なやりかたで組み合わせるということ", \t -> do
	text t "* 型は関数を組み合わせる際に重要な役割を持つ", \t -> do
	text t "* プログラミングの機能として「くりかえし」は重要", \t -> do
	text t "* 「くりかえし」を実現するために", \t -> do
	itext t 1 "- 手続き型言語では状態変化を使う", \t -> do
	itext t 1 "- Haskellでは主にリストを使う", \t -> do
	text t "* 今回は関数・型・リストについて学ぼう"
 ]

function :: Page
function = [\t -> do
	writeTopTitle t "関数の定義"
	text t "", \t -> do
	text t "* C言語では以下のようにする", \t -> do
	itext t 1 "int add(int x, int y) { return x + y; }", \t -> do
	text t "* 同じことをHaskellでは以下のようにする", \t -> do
	itext t 1 "add x y = x + y", \t -> do
	text t "* [関数名] [仮引数1] [仮引数2] ... = [表現]という形", \t -> do
	text t "* '='を使っているのは何故か?", \t -> do
	itext t 1 "add x yはx + yに置き換えられるということ", \t -> do
	itext t 1 "プログラム中のadd 3 8は3 + 8に置き換え可能"
 ]

apply :: Page
apply = [\t -> do
	writeTopTitle t "関数の適用"
	text t "", \t -> do
	text t "* 「関数の定義」でもすこし触れたが", \t -> do
	text t "* addの仮引数x, yに実引数3, 8を入れるには以下のように", \t -> do
	itext t 1 "add 3 8", \t -> do
	text t "* [関数名] [実引数1] [実引数2] ...という形", \t -> do
	text t "* 関数適用の際に関数名のあとに(..., ...)のようにしない", \t -> do
	text t "* 単に引数を並べれば良い", \t -> do
	text t "* この記法の合理性については後々明らかになる"
 ]

defineFun :: Page
defineFun = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* ghciの対話環境内でも関数を定義できる", \t -> do
	text t "* しかし、関数が長くなるとわずらわしいので", \t -> do
	text t "* 別ファイルで関数を定義し、", \t -> do
	itext t 1 "対話環境でそれを使うことにしよう", \t -> do
	text t "* お好きなエディタを選んで、と言いたいところだが", \t -> do
	itext t 1 "- メモ帳、Vim、Emacsしか用意していない"
	itext t 2 "(著者メモ) Emacsは用意できるかどうか"
 ]

defineFun2 :: Page
defineFun2 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 適当なフォルダを作る", \t -> do
	itext t 1 "% cd ~ (Linuxの場合)"
	itext t 1 "% cd %userprofile% (Windowsの場合)", \t -> do
	itext t 1 "% mkdir lectures/lecture01", \t -> do
	itext t 1 "% cd lectures/lecture01", \t -> do
	text t "* 例題: 身長と体重を入力するとBMIを返す関数bmiを作る", \t -> do
	itext t 1 "BMI = [体重(kg)] / [身長(m)]の2乗", \t -> do
	text t "* bmi.hsファイルを作ろう", \t -> do
	itext t 1 "% [エディタ] bmi.hs", \t -> do
	itext t 2 "[エディタ]はnotepad, vim, emacsのどれか"
 ]

bmi h w = w / (h / 100) ^ 2

defineFun3 :: Page
defineFun3 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 以下の内容を書き込む", \t -> do
	itext t 1 "bmi h w = w / (h / 100) ^ 2", \t -> do
	text t "* ghciにこのファイルを読み込ませる", \t -> do
	itext t 1 "% ghci bmi.hs", \t -> do
	itext t 1 "*Main> ", \t -> do
	itext t 2 "- \"Prelude>\"ではなく\"*Main>\"になった", \t -> do
	itext t 2 "- 理由については後々明らかになる", \t -> do
	text t "* とりあえずチェ・ホンマンのBMIを求めてみよう", \t -> do
	itext t 1 "*Main> bmi 218 160", \t -> do
	itext t 1 $ show $ bmi 218 160
 ]

funSummary :: Page
funSummary = [\t -> do
	writeTopTitle t "関数(まとめ)"
	text t "", \t -> do
	text t "* 関数とは入力を取り出力を返すもの", \t -> do
	text t "* Haskellでは以下のように定義する", \t -> do
	itext t 1 "[関数名] [仮引数1] [仮引数2] ... = [表現]", \t -> do
	text t "* 関数fに値xを与えることをxにfを適用するという", \t -> do
	text t "* 関数適用は以下のようにする", \t -> do
	itext t 1 "[関数名] [実引数1] [実引数2] ..."
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
