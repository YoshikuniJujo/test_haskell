import System.Random
import System.IO.Unsafe
import Data.Char

import Lecture

subtitle :: String
subtitle = "第2回 型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	{- aboutType, aboutType2, aboutType3, -} aboutType4, aboutType5,
	aboutType6, aboutType7, aboutType9, aboutType8, aboutType10,
	aboutType11, aboutType12, aboutType13, aboutType14, operator,
	polymorphism, polymorphism2, polymorphism3, polymorphism4,
	polymorphism5, polymorphism6,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 値には型がある", \t -> do
	text t "* 型は以下を表す", \t -> do
	itext t 1 "- その値にどの関数を適用できるか", \t -> do
	itext t 1 "- どの関数がその値を返すか", \t -> do
	text t "* つまり、型はその値で「何ができるか」を表す", \t -> do
	text t "* 関数にも型があり、入力と出力の型で定義される", \t -> do
	text t "* 複数の型に適用できる関数があり多相関数と呼ばれる", \t -> do
	text t "* 多相関数の型は型変数を使って表現する", \t -> do
	text t "* 型の導出はHaskellを学ぶ際のよい練習になる"
 ]


at4char1 :: Char
at4char1 = unsafePerformIO $ randomRIO ('A', 'Z')

aboutType4 :: Page
aboutType4 = [\t -> do
	writeTopTitle t "「何ができるか」"
	text t "", \t -> do
	text t "* 値には型があり、型によって「何ができるか」が決まる", \t -> do
	text t "* Haskellには文字を表すChar型が用意されている", \t -> do
	text t "* 文字はtoLowerで小文字にすることができる", \t -> do
	itext t 1 "Prelude> :m Data.Char", \t -> do
	itext t 2 "- toLowerを含むData.Charモジュールの読み込み", \t -> do
	itext t 1 $ "Prelude Data.Char> toLower " ++ show at4char1, \t -> do
	itext t 1 $ show $ toLower at4char1
 ]

at5bool1 :: Bool
at5bool1 = unsafePerformIO randomIO

aboutType5 :: Page
aboutType5 = [\t -> do
	writeTopTitle t "型の不適合"
	text t "", \t -> do
	text t "* 真偽を表現するBool型がある", \t -> do
	itext t 1 "- False, Trueの2つの値のみを持つ", \t -> do
	text t $ "* " ++ show at5bool1 ++ "を小文字にすることはできない", \t -> do
	itext t 1 $ "Prelude Data.Char> toLower " ++ show at5bool1, \t -> do
	itext t 1 "..."
	itext t 1 "Couldn't match expected type `Char' with"
	itext t 1 "actual type `Bool'"
	itext t 1 $ "In the first argument of `toLower', namely `" ++ show at5bool1 ++ "'"
	itext t 1 "...", \t -> do
	text t $ "* 「" ++ show at5bool1 ++ "はBoolであってCharじゃないよ」と言われた"
 ]

aboutType6 :: Page
aboutType6 = [\t -> do
	writeTopTitle t "関数の型"
	text t "", \t -> do
	text t "* 関数の型は入力の型と出力の型によって決まる", \t -> do
	text t "* たとえばtoLowerは文字を取って文字を返す関数で", \t -> do
	itext t 1 "型はChar -> Charとなる", \t -> do
	text t "* 例の関数bmiの型はどうなるだろうか", \t -> do
	text t "* 入力と出力を倍精度浮動小数点数とする", \t -> do
	text t "* 倍精度浮動小数点数を表す型はDoubleなので", \t -> do
	text t "* bmiはDoubleを2こ取ってDoubleを返す関数", \t -> do
	text t "* 2引数関数は本当は「関数を返す関数」なので", \t -> do
	itext t 1 "Double -> (Double -> Double)", \t -> do
	text t "* (->)は右結合なので()は省略できる", \t -> do
	itext t 1 "Double -> Double -> Double"
 ]

aboutType7 :: Page
aboutType7 = [\t -> do
	writeTopTitle t "関数の型"
	text t "", \t -> do
	text t "* 関数の型は以下のように表される", \t -> do
	itext t 1 "[引数1の型] -> [引数2の型] -> ... -> [返り値の型]"
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
	text t "* lectures/lecture02/bmi.hsを作ろう", \t -> do
	itext t 1 "% cd ~/lectures/lecture02", \t -> do
	itext t 1 "% nano -w bmi.hs", \t -> do
	itext t 1 "bmi :: Double -> Double -> Double"
	itext t 1 "bmi = undefined"
	itext t 1 ""
	itext t 1 "isObese :: Double -> Double -> Bool"
	itext t 1 "isObese = undefined", \t -> do
	text t "* undefinedは評価されるとエラーとなる値", \t -> do
	text t "* スタブとして使用した"
 ]

aboutType11 :: Page
aboutType11 = [\t -> do
	writeTopTitle t "型駆動開発", \t -> do
	text t "* エディタとは別のターミナルで", \t -> do
	itext t 0 "% cd ~/lectures/lecture02", \t -> do
	itext t 0 "% ghci bmi", \t -> do
	itext t 0 "*Main> bmi 218 164", \t -> do
	itext t 0 "*** Exception: Prelude.undefined", \t -> do
	itext t 0 "*Main> isObese 218 164", \t -> do
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
	text t "* 今回はトップダウンで開発する", \t -> do
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
	text t "* よってbmi関数を定義すれば仕事は終わる", \t -> do
	itext t 1 "bmi h w = w / (h / 100) ^ 2"
 ]

aboutType14 :: Page
aboutType14 = [\t -> do
	writeTopTitle t "型駆動開発"
	text t "", \t -> do
	text t "* 動かしてみよう", \t -> do
	itext t 1 "% ghci bmi.hs", \t -> do
	itext t 1 "*Main> isObese 218 164", \t -> do
	itext t 1 "True", \t -> do
	itext t 1 "*Main> isObese 218 140", \t -> do
	itext t 1 "False", \t -> do
	text t "* チェ・ホンマンはかつて肥満であったと判定された、ただし", \t -> do
	itext t 1 "- BMIは高身長者には厳しい", \t -> do
	itext t 1 "- 脂肪と筋肉を区別できない"
 ]

operator :: Page
operator = [\t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* 演算子についても型宣言ができる", \t -> do
	text t "* ()でくくって「関数」にすればよい", \t -> do
	text t "* (&&)演算子を見てみよう", \t -> do
	itext t 1 "*Main> False && True", \t -> do
	itext t 1 $ show $ False && True, \t -> do
	itext t 1 "*Main> True && True", \t -> do
	itext t 1 $ show $ True && True, \t -> do
	text t "* Bool値を2つとりBool値を返す関数(演算子)なので", \t -> do
	itext t 1 "(&&) :: Bool -> Bool -> Bool"
 ]

pm1char1 :: Char
pm1char1 = unsafePerformIO $ randomRIO ('a', 'z')
pm1bool1 :: Bool
pm1bool1 = unsafePerformIO randomIO

polymorphism :: Page
polymorphism = [\t -> do
	writeTopTitle t "多相関数"
	text t "", \t -> do
	text t "* 複数の型に適用可能な関数がある", \t -> do
	text t "* 非常に単純な関数としてidがある", \t -> do
	itext t 1 $ "*Main> id " ++ show pm1char1, \t -> do
	itext t 1 $ show $ id pm1char1, \t -> do
	itext t 1 $ "*Main> id " ++ show pm1bool1, \t -> do
	itext t 1 $ show $ id pm1bool1, \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "id x = x", \t -> do
	text t "* つまり引数をそのまま返す関数である", \t -> do
	text t "* 引数が何であれ自分自身を返すことは可能", \t -> do
	arrowIText t 1 "すべての型の値を引数とすることができる"
 ]

polymorphism2 :: Page
polymorphism2 = [\t -> do
	writeTopTitle t "多相関数"
	text t "", \t -> do
	text t "* idの型宣言を書きたい", \t -> do
	text t $ "* id " ++ show pm1char1 ++ "のときはidの型は", \t -> do
	itext t 1 "Char -> Char", \t -> do
	text t $ "* id " ++ show pm1bool1 ++ "のときはidの型は", \t -> do
	itext t 1 "Bool -> Bool", \t -> do
	text t "* つまり[型X] -> [型X]という形", \t -> do
	text t "* このようなとき型変数が使える", \t -> do
	text t "* 型変数は小文字ではじまる識別子である", \t -> do
	text t "* よって以下のようにできる", \t -> do
	itext t 1 "id :: a -> a", \t -> do
	text t "* idは型aの値をとり、同じ型aの値を返す関数"
 ]

pm3int1, pm3int2 :: Int
[pm3int1, pm3int2] = unsafePerformIO $ mapM randomRIO [(97, 122), (65, 90)]

polymorphism3 :: Page
polymorphism3 = [\t -> do
	writeTopTitle t "多相関数"
	text t "", \t -> do
	text t "* (.)の型について見ていこう", \t -> do
	text t "* 例として以下の関数を使う", \t -> do
	itext t 1 "chr :: Int -> Char", \t -> do
	itext t 1 "isLower :: Char -> Bool", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "*Main> :m Data.Char", \t -> do
	itext t 1 $ "Prelude Data.Char> chr " ++ show pm3int1, \t -> do
	itext t 1 $ show $ chr pm3int1, \t -> do
	itext t 1 $ "Prelude Data.Char> chr " ++ show pm3int2, \t -> do
	itext t 1 $ show $ chr pm3int2
 ]

polymorphism4 :: Page
polymorphism4 = [\t -> do
	writeTopTitle t "多相関数"
	text t "", \t -> do
	itext t 1 $ "Prelude Data.Char> (isLower . chr) " ++ show pm3int1, \t -> do
	itext t 1 $ show $ (isLower . chr) pm3int1, \t -> do
	itext t 1 $ "Prelude Data.Char> (isLower . chr) " ++ show pm3int2, \t -> do
	itext t 1 $ show $ (isLower . chr) pm3int2, \t -> do
	text t "* それぞれの型を見ると", \t -> do
	itext t 1 "isLower :: Char -> Bool", \t -> do
	itext t 1 "chr :: Int -> Char", \t -> do
	itext t 1 "isLower . chr :: Int -> Bool"
 ]

polymorphism5 :: Page
polymorphism5 = [\t -> do
	writeTopTitle t "多相関数"
	text t "", \t -> do
	text t "* (.)への入力と出力として考えると", \t -> do
	itext t 1 "入力1: Char -> Bool", \t -> do
	itext t 1 "入力2: Int -> Char", \t -> do
	itext t 1 "出力 : Int -> Bool", \t -> do
	text t "* よって(isLower . chr)のときの(.)の型は", \t -> do
	itext t 1 "(Char -> Bool) -> (Int -> Char) -> (Int -> Bool)", \t -> do
	text t "* 次は、より一般的な場合について見てみよう"
 ]

polymorphism6 :: Page
polymorphism6 = [\t -> do
	writeTopTitle t "多相関数"
	text t "", \t -> do
	text t "* (.)の定義を見てみよう", \t -> do
	itext t 1 "(.) f g = \\x -> f (g x)", \t -> do
	text t "* xの型をaとするとgの型は(a -> b)となる", \t -> do
	text t "* (g x)の型がbとなるのでfの型は(b -> c)となる", \t -> do
	text t "* f (g x)の型はcとなるので", \t -> do
	itext t 1 "\\x -> f (g x)の型はa -> cとなる", \t -> do
	text t "* まとめると", \t -> do
	itext t 1 "f :: b -> c", \t -> do
	itext t 1 "g :: a -> b", \t -> do
	itext t 1 "\\x -> f (g x) :: a -> c", \t -> do
	text t "* よって(.) :: (b -> c) -> (a -> b) -> (a -> c)"
 ]

higherOrder :: Page
higherOrder = [\t -> do
	writeTopTitle t "高階関数"
	text t ""
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 型とは値の種類", \t -> do
	text t "* 型が同じなら値によらずできることは同じ", \t -> do
	text t "* 関数を作るときにはまず型を考える", \t -> do
	itext t 1 "- 型が決まれば仕事は半分終わったようなもの", \t -> do
	itext t 1 "- 型チェックによってバグの多くは検出可", \t -> do
	itext t 1 "- 型はドキュメントとしても優れる", \t -> do
	text t "* 型の宣言は[関数名] :: [型]という形", \t -> do
	text t "* 多相関数の型宣言には型変数を使う", \t -> do
	text t "* 型の導出は勉強になる"
 ]
