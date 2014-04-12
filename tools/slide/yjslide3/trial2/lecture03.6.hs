import Lecture

import Data.Char

subtitle :: String
subtitle = "トライアル 第3.6回 いろいろな多相関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	funId, funId2, funId3, funConst, funConst2, funConst3,
	funConstId, funConstId2, funConstId3,
	dollar, funFlip, funFlipConst, dot, dot2, dot3, dot4, dot5, dot6,
	funConvert, funConvert2, funConvert3, funConvert4, adaptor,
	funCurry, funCurry2, funCurry3, funCurry4,
	funUncurry, funUncurry2,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 関数を理解することがHaskellを理解する鍵である", \t -> do
	text t "* Haskellでは高階関数を多用する", \t -> do
	text t "* 高階関数の多くは多相関数である", \t -> do
	text t "* 高階関数について理解するには", \t -> do
	itext t 1 "何よりも「慣れ」が必要である", \t -> do
	text t "* 今までの思考の習慣を打破しなければならない", \t -> do
	text t "* 簡単なものから関数について見ていこう"
 ]

-- id, ($), const, flip, curry, uncurry, (.)

funId :: Page
funId = [\t -> do
	writeTopTitle t "id"
	text t "", \t -> do
	text t "* まずは関数idについて見てみよう", \t -> do
	text t "* この関数は引数をそのまま返す関数", \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "id :: a -> a", \t -> do
	itext t 1 "id x = x", \t -> do
	text t "* 使い道はなさそうだが", \t -> do
	itext t 1 "慣れてくるとけっこう「使える」関数である"
 ]

devide :: Double -> Double -> Double
devide 0 = id
devide d = (/ d)

fi2double0, fi2double1, fi2double2, fi2double3 :: Double
[fi2double0, fi2double1, fi2double2, fi2double3] =
	[4, 7, 0, 9]

funId2 :: Page
funId2 = [\t -> do
	writeTopTitle t "id"
	text t "", \t -> do
	text t "* idの使用例を挙げてみる", \t -> do
	text t "* 第1引数が0ならばそのままの値を返し", \t -> do
	text t "* 0でなければ第1引数で第2引数を割った値を返す関数", \t -> do
	itext t 1 "% cd ~/lectures/lecture01/", \t -> do
	itext t 1 "% nano -w functions.hs", \t -> do
	itext t 1 "devide :: Double -> Double -> Double", \t -> do
	itext t 1 "devide 0 = id", \t -> do
	itext t 1 "devide d = (/ d)"
 ]

funId3 :: Page
funId3 = [\t -> do
	writeTopTitle t "id"
	text t "", \t -> do
	text t "* 対話環境用のターミナルで", \t -> do
	itext t 1 "% ghci functions.hs", \t -> do
	itext t 1 $ "*Main> devide " ++ show fi2double0 ++ " " ++ show fi2double1, \t -> do
	itext t 1 $ show $ devide fi2double0 fi2double1, \t -> do
	itext t 1 $ "*Main> devide " ++ show fi2double2 ++ " " ++ show fi2double3, \t -> do
	itext t 1 $ show $ devide fi2double2 fi2double3
 ]

funConst :: Page
funConst = [\t -> do
	writeTopTitle t "const"
	text t "", \t -> do
	text t "* 引数を無視して決まった値を出力する関数について考える", \t -> do
	text t "* たとえば以下のようなconst0を考えてみよう", \t -> do
	itext t 1 "const0 :: Double -> Double", \t -> do
	itext t 1 "const0 _ = 0", \t -> do
	text t "* 同じような関数はいろいろと考えられる", \t -> do
	itext t 1 "constA :: Int -> Char", \t -> do
	itext t 1 "constA _ = 'A'", \t -> do
	itext t 1 "constTrue :: a -> Bool", \t -> do
	itext t 1 "constTrue _ = True"
 ]

funConst2 :: Page
funConst2 = [\t -> do
	writeTopTitle t "const"
	text t "", \t -> do
	text t "* これらは構造が同じで値が違うだけである", \t -> do
	text t "* 構造が同じで値が違うだけの関数は", \t -> do
	itext t 1 "メタ的な関数でまとめることができる", \t -> do
	text t "* つまり「一定の値」を与えると", \t -> do
	itext t 1 "「どの値に対してもその値を返す関数」を返す関数", \t -> do
	itext t 1 "const :: a -> (b -> a)", \t -> do
	itext t 1 "const c = \\_ -> c", \t -> do
	text t "* この関数も使い道がないように思えるが、そうではない", \t -> do
	text t "* さっきのdevideは第1引数が0ならば第2引数をそのまま返す", \t -> do
	text t "* 第1引数が0のときには0を返すdevide'を考えてみよう"
 ]

devide' :: Double -> Double -> Double
devide' 0 = const 0
devide' d = (/ d)

fc3double0, fc3double1, fc3double3, fc3double4 :: Double
[fc3double0, fc3double1, fc3double3, fc3double4] =
	[8, 5, 0, 9]

funConst3 :: Page
funConst3 = [\t -> do
	writeTopTitle t "const"
	text t "", \t -> do
	text t "* devide'は以下のようになるだろう", \t -> do
	itext t 1 "devide' :: Double -> Double -> Double", \t -> do
	itext t 1 "devide' 0 = const 0", \t -> do
	itext t 1 "devide' d = (/ d)", \t -> do
	text t "* function.hsに書き込んで、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> devide' " ++ show fc3double0 ++ " " ++ show fc3double1, \t -> do
	itext t 1 $ show $ devide' fc3double0 fc3double1, \t -> do
	itext t 1 $ "*Main> devide' " ++ show fc3double3 ++ " " ++ show fc3double4, \t -> do
	itext t 1 $ show $ devide' fc3double3 fc3double4
 ]

funConstId :: Page
funConstId = [\t -> do
	writeTopTitle t "const id"
	text t "", \t -> do
	text t "* 多くの関数は複数の解釈が可能である", \t -> do
	text t "* constは「引数に関らず一定の値を返す関数」を返す関数", \t -> do
	text t "* constは引数を2つ取り第1引数の値を返す関数", \t -> do
	text t "* 同じことだがそれぞれの意図を強調すると", \t -> do
	itext t 1 "const c = \\_ -> c", \t -> do
	itext t 1 "const x _y = x", \t -> do
	text t "* ここで「引数を2つ取り第2引数の値を返す関数」を考える", \t -> do
	itext t 1 "sndArg :: a -> b -> b", \t -> do
	itext t 1 "sndArg _x y = y"
 ]

sndArg :: a -> b -> b
sndArg = const id

fc2Int0 :: Int
fc2Int0 = 8

fc2Char0 :: Char
fc2Char0 = 'c'

funConstId2 :: Page
funConstId2 = [\t -> do
	writeTopTitle t "const id"
	text t "", \t -> do
	text t "* この関数は「引数に関らず常にidを返す関数」とも読める", \t -> do
	itext t 1 "sndArg :: a -> (b -> b)", \t -> do
	itext t 1 "sndArg _x = \\y -> y", \t -> do
	text t "* よってconstとidを使って以下のように定義できる", \t -> do
	itext t 1 "sndArg = const id"
 ]

funConstId3 :: Page
funConstId3 = [\t -> do
	writeTopTitle t "const id"
	text t "", \t -> do
	text t "* 以下をfunctions.hsに書き込もう", \t -> do
	itext t 1 "sndArg :: a -> b -> b", \t -> do
	itext t 1 "sndArg = const id", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> sndArg " ++ show fc2Int0 ++ " " ++ show fc2Char0, \t -> do
	itext t 1 $ show $ sndArg fc2Int0 fc2Char0
 ]

funFlip :: Page
funFlip = [\t -> do
	writeTopTitle t "flip"
	text t "", \t -> do
	text t "* 2引数関数の引数の位置を入れ換える関数flipがある", \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "flip :: (a -> b -> c) -> b -> a -> c", \t -> do
	itext t 1 "flip f x y = f y x", \t -> do
	text t "* 第1引数の関数に第3引数、第2引数をこの順で与えている", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> (/) 3 2", \t -> do
	itext t 1 $ show $ (/) (3 :: Double) 2, \t -> do
	itext t 1 "*Main> flip (/) 2 3", \t -> do
	itext t 1 $ show $ flip (/) (2 :: Double) 3
 ]

sndArg' :: a -> b -> b
sndArg' = flip const

ffcInt0 :: Int
ffcInt0 = 7

ffcChar0 :: Char
ffcChar0 = 'd'

funFlipConst :: Page
funFlipConst = [\t -> do
	writeTopTitle t "flip const"
	text t "", \t -> do
	text t "* constは第2引数を無視して第1引数を返す", \t -> do
	text t "* constの引数を入れ換えれば", \t -> do
	itext t 1 "第1引数を無視して第2引数を返す関数が作れる", \t -> do
	text t "* sndArg'を定義してみよう", \t -> do
	itext t 1 "sndArg' :: a -> b -> b", \t -> do
	itext t 1 "sndArg' = flip const", \t -> do
	text t "* functions.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> sndArg' " ++ show ffcInt0 ++ " " ++ show ffcChar0, \t -> do
	itext t 1 $ show $ sndArg' ffcInt0 ffcChar0
 ]

dollar :: Page
dollar = [\t -> do
	writeTopTitle t "($)"
	text t "", \t -> do
	text t "* $演算子について見てみよう", \t -> do
	itext t 1 "($) :: (a -> b) -> a -> b", \t -> do
	itext t 1 "($) f x = f x", \t -> do
	text t "* 第1引数の関数に第2引数の値を与えている", \t -> do
	text t "* あるいは第2引数の値に第1引数の関数を適用", \t -> do
	text t "* 「関数と値を取って値に関数を適用」というパターンは", \t -> do
	itext t 1 "様々な関数のなかに認められる", \t -> do
	text t "* ($)をその原型としてイメージを作っておくと良い"
--	text t "* 「関数適用を行う関数」とも呼べるが", \t -> do
--	itext t 1 "関数自体に「関数適用」の機能はあるので", \t -> do
--	itext t 1 "($) f == f"
 ]

dot :: Page
dot = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* ドット演算子と呼ばれる演算子(関数)がある", \t -> do
	itext t 1 "(.) :: (b -> c) -> (a -> b) -> a -> c", \t -> do
	itext t 1 "(.) f g x = f (g x)", \t -> do
	text t "* 3つの引数f, g, xを取り", \t -> do
	itext t 1 "値xに関数gを適用し、その結果に関数fを適用する関数"
 ]

dot2 :: Page
dot2 = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* 使用例として以下の関数を考える", \t -> do
	itext t 1 "「文字を小文字にしてその文字コードを調べる」", \t -> do
	text t "* (.)を使わずに書くと以下のようになるだろう", \t -> do
	itext t 1 "lowerOrd :: Char -> Int", \t -> do
	itext t 1 "lowerOrd c = ord (toLower c)", \t -> do
	text t "* (.)の定義を再掲する", \t -> do
	itext t 1 "(.) f g x = f (g x)", \t -> do
	text t "* lowerOrdの右辺を(.)を使って書き直してみる", \t -> do
	itext t 1 "(.) ord toLower c", \t -> do
	text t "* (.)は、cにtoLowerを適用し、その結果にordを適用する"
 ]

dot3 :: Page
dot3 = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* lowerOrdを(.)を使って定義すると以下のようになる", \t -> do
	itext t 1 "lowerOrd c = (.) ord toLower c", \t -> do
	text t "* (.) ord toLower cは((.) ord toLower) cということ", \t -> do
	text t "* よってcを消して、以下のようにできる", \t -> do
	itext t 1 "lowerOrd = (.) ord toLower", \t -> do
	text t "* 中置記法に直すと", \t -> do
	itext t 1 "lowerOrd = ord . toLower", \t -> do
	text t "* 「ある関数の結果を別の関数にわたす関数の作成」を", \t -> do
	itext t 1 "関数合成と呼ぶ", \t -> do
	text t "* (.)は関数を合成する演算子と考えることができる"
 ]

lowerOrd :: Char -> Int
lowerOrd = ord . toLower

dot4 :: Page
dot4 = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* lowerOrdはData.Charモジュールの関数を使っている", \t -> do
	text t "* functions.hsの先頭に以下を書き込もう", \t -> do
	itext t 1 "import Data.Char (toLower, ord)", \t -> do
	text t "* 以下を追加する", \t -> do
	itext t 1 "lowerOrd :: Char -> Int", \t -> do
	itext t 1 "lowerOrd = ord . toLower", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> lowerOrd 'K'", \t -> do
	itext t 1 $ show $ lowerOrd 'K'
 ]

dot5 :: Page
dot5 = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* (.)を中置演算子として使うことで", \t -> do
	itext t 1 "「fをしてgをしてhをして...」という形が簡潔になる", \t -> do
	text t "* 「小文字にして文字コードを求めて"
	itext t 1 "それに2足した結果を文字にする」関数を書く", \t -> do
	itext t 1 "lowerPlus2 :: Char -> Char", \t -> do
	itext t 1 "lowerPlus2 = chr . (+ 2) . ord . toLower", \t -> do
	text t "* 上記をfunctions.hsに書き込み"
	itext t 1 "インポートリストにchrを追加する", \t -> do
	itext t 1 "import Data.Char (toLower, ord, chr)"
 ]

lowerPlus2 :: Char -> Char
lowerPlus2 = chr . (+ 2) . ord . toLower

dot6 :: Page
dot6 = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> lowerPlus2 'J'", \t -> do
	itext t 1 $ show $ lowerPlus2 'J', \t -> do
	itext t 1 "*Main> lowerPlus2 'Y'", \t -> do
	itext t 1 $ show $ lowerPlus2 'Y'
 ]

half :: Double -> Double
half = (/ 2)

eight :: Int
eight = 8

funConvert :: Page
funConvert = [\t -> do
	writeTopTitle t "convert"
	text t "", \t -> do
	text t "* 引数としてDoubleを取る関数にIntを与えることを考える", \t -> do
	text t "* IntからDoubleへの変換にはfromIntegralが使える", \t -> do
	itext t 1 "*Main> fromIntegral 3 :: Double", \t -> do
	itext t 1 $ show (fromIntegral (3 :: Int) :: Double), \t -> do
	text t "* 以下の関数を考えよう", \t -> do
	itext t 1 "half :: Double -> Double", \t -> do
	itext t 1 "half = (/ 2)", \t -> do
	text t "* functions.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> half 3.4", \t -> do
	itext t 1 $ show $ half 3.4
 ]

funConvert2 :: Page
funConvert2 = [\t -> do
	writeTopTitle t "convert"
	text t "", \t -> do
	text t "* これにInt型の整数を与えてみる", \t -> do
	text t "* Int型の整数eightを定義しておく", \t -> do
	itext t 1 "eight :: Int", \t -> do
	itext t 1 "eight = 8", \t -> do
	text t "* functions.hsに書き込み、試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> half eight", \t -> do
	itext t 1 "...Couldn't match expected type `Double' with"
	itext t 1 "actual type `Int'...", \t -> do
	text t "* 型エラーとなる"
 ]

funConvert3 :: Page
funConvert3 = [\t -> do
	writeTopTitle t "convert"
	text t "", \t -> do
	text t "* eightの型を変換してやることで適用可能となる", \t -> do
	itext t 1 "*Main> half (fromIntegral eight)", \t -> do
	itext t 1 $ show $ half (fromIntegral eight), \t -> do
	text t "* 関数と値の型を合致させるために値の型を変換した", \t -> do
	text t "* 逆に関数の型のほうを変換してやってもいい", \t -> do
	text t "* 関数の引数の型をDoubleからIntに変換する関数を考える", \t -> do
	itext t 1 "convert :: (Double -> Double) -> (Int -> Double)", \t -> do
	text t "* (->)は右結合なので型宣言から()を省略できる", \t -> do
	itext t 1 "convert :: (Double -> Double) -> Int -> Double", \t -> do
	itext t 1 "convert f n = f (fromIntegral n)", \t -> do
	text t "* これをfunctions.hsに書き込もう"
 ]

convert :: (Double -> Double) -> Int -> Double
convert f n = f (fromIntegral n)

funConvert4 :: Page
funConvert4 = [\t -> do
	writeTopTitle t "convert"
	text t "", \t -> do
	text t "* convertを使えば以下のようにすることができる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> (convert half) eight", \t -> do
	itext t 1 $ show $ (convert half) eight, \t -> do
	text t "* convert halfはeight以外のInt型の値にも適用可能である", \t -> do
	text t "* 値を変換する代わりに関数側を変換するという手法", \t -> do
	text t "* アダプタを値の側につけるか関数側につけるかということ", \t -> do
	text t "* アダプタのメタファーを直接的に表現してみると", \t -> do
	itext t 1 "*Main> half (fromIntegral eight)", \t -> do
	itext t 1 $ show $ half (fromIntegral eight), \t -> do
	itext t 1 "*Main> (half . fromIntegral) eight", \t -> do
	itext t 1 $ show $ (half . fromIntegral) eight
 ]

adaptor :: Page
adaptor = [\t -> do
	writeTopTitle t "変換アダプタ"
	text t "", \t -> do
	text t "* 変換アダプタをイメージする", \t -> do
	text t "* 関数の引数の型と値の型とが異なる場合", \t -> do
	itext t 1 "変換アダプタとして型を変換する関数が必要になる", \t -> do
	text t "* その変換アダプタを値の側に接続しても良いが", \t -> do
	itext t 1 "関数側に接続しても良い", \t -> do
	text t "* 関数側に接続するためには", \t -> do
	itext t 1 "その関数と変換関数とを「関数合成」してやれば良い", \t -> do
	text t "* さっきの例を示す", \t -> do
	itext t 1 "half (fromIntegral eight)", \t -> do
	itext t 1 "(half . fromIntegral) eight", \t -> do
	text t "* 引数の型をDoubleからIntに変換するために", \t -> do
	itext t 1 "IntからDoubleへの変換関数を使っている"
 ]

absTup :: (Double, Double) -> Double
absTup (x, y) = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)

absCrr :: Double -> Double -> Double
absCrr x y = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)

funCurry :: Page
funCurry = [\t -> do
	writeTopTitle t "curry"
	text t "", \t -> do
	text t "* 以下のようなabsTupをfunctions.hsに書き込もう", \t -> do
	itext t 1 "absTup :: (Double, Double) -> Double", \t -> do
	itext t 1 "absTup (x, y) = sqrt $ x ^ 2 + y ^ 2", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> absTup (3, 4)", \t -> do
	itext t 1 $ show $ absTup (3, 4), \t -> do
	text t "* x = 3と固定したうえでyの値をいろいろと変化させる", \t -> do
	text t "* そのための関数を作りたい", \t -> do
	text t "* xの値だけを部分適用したいということ", \t -> do
	text t "* しかしxはタプルの一部なので部分適用はできない"
 ]

funCurry2 :: Page
funCurry2 = [\t -> do
	writeTopTitle t "curry"
	text t "", \t -> do
	text t "* 部分適用するために関数の型を以下のように変換する", \t -> do
	itext t 1 "(Double, Double) -> Double", \t -> do
	arrowIText t 1 "Double -> Double -> Double", \t -> do
	text t "* それを行う関数curryが定義されている", \t -> do
	itext t 1 "curry :: ((a, b) -> c) -> a -> b -> c", \t -> do
	itext t 1 "curry f x y = f (x, y)", \t -> do
	text t "* 引数の型をタプルからばらばらの値に変換するために", \t -> do
	itext t 1 "ばらばらの値をタプルにまとめている", \t -> do
	text t "* 「変換アダプタ」のスライドを思い出してみよう", \t -> do
	text t "* 関数の引数についての変換は逆向きの変換を使う"
 ]

funCurry3 :: Page
funCurry3 = [\t -> do
	writeTopTitle t "curry"
	text t "", \t -> do
	text t "* curry関数の説明をさらに続けよう", \t -> do
	itext t 1 "curry f x y = f (x, y)", \t -> do
	text t "* curryは3引数f, x, yを取り", \t -> do
	itext t 1 "値xと値yからタプルを作成し、それにfを適用する", \t -> do
	text t "* ここでcurry fという部分を見てみると", \t -> do
	itext t 1 "「関数fと同じことをばらばらの値に対して行う関数」", \t -> do
	text t "* 関数の引数をDoubleからIntに変換するためには", \t -> do
	itext t 1 "値をIntからDoubleに変換してやる必要がある", \t -> do
	text t "* 関数の引数をタプルからばらばらの値にするためには", \t -> do
	itext t 1 "ばらばらの値をタプルにまとめてやる必要がある", \t -> do
	text t "* 引数部分に対する処理は「逆」の処理になる"
 ]

abs3 :: Double -> Double
abs3 = curry absTup 3

funCurry4 :: Page
funCurry4 = [\t -> do
	writeTopTitle t "curry"
	text t "", \t -> do
	text t "* x = 3と固定したうえでベクトルの絶対値を求める関数", \t -> do
	itext t 1 "abs3 :: Double -> Double", \t -> do
	itext t 1 "abs3 = curry absTup 3", \t -> do
	text t "* functions.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> abs3 4", \t -> do
	itext t 1 $ show $ abs3 4
 ]

funUncurry :: Page
funUncurry = [\t -> do
	writeTopTitle t "uncurry"
	text t "", \t -> do
	text t "* curryと逆の変換を行う関数uncurryがある", \t -> do
	text t "* 例えば以下のような2組の数があるとする", \t -> do
	itext t 1 "pair :: (Int, Int)", \t -> do
	itext t 1 "pair = (2, 8)", \t -> do
	text t "* この2数を足し合わせたい", \t -> do
	text t "* このようなときにuncurryが使える", \t -> do
	text t "* uncurryはばらばらの引数をひとつのタプルにまとめる", \t -> do
	itext t 1 "uncurry :: (a -> b -> c) -> (a, b) -> c", \t -> do
	itext t 1 "uncurry f (x, y) = f x y", \t -> do
	text t "* uncurryは引数として関数fとタプルを取り", \t -> do
	itext t 1 "タプルの要素をばらばらにして、それに関数fを適用"
 ]

pair :: (Int, Int)
pair = (2, 8)

funUncurry2 :: Page
funUncurry2 = [\t -> do
	writeTopTitle t "uncurry"
	text t "", \t -> do
	text t "* さっきのpairをfunctions.hsに書き込もう", \t -> do
	itext t 1 "pair :: (Int, Int)", \t -> do
	itext t 1 "pair = (2, 8)", \t -> do
	text t "試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> uncurry (+) pair", \t -> do
	itext t 1 $ show $ uncurry (+) pair
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellにおける基本的な関数を見てきた", \t -> do
	text t "* 今回見てきた関数はどれも「構造」を扱う関数と言える", \t -> do
	text t "* 一般性のある関数によって「構造」を変化させていく手法", \t -> do
	text t "* Haskellでは関数の変換は非常に安価である", \t -> do
	text t "* 関数の引数の位置を変えたり、タプルにまとめたりできる", \t -> do
	text t "* 既存の関数の構造を変えていくことで様々なことができる"
 ]
