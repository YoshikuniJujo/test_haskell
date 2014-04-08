import Lecture

import Data.Char

subtitle :: String
subtitle = "トライアル 第3.6回 いろいろな関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	funId, funId2, funId3, funConst, funConst2, funConst3,
	funConstId, funConstId2, funConstId3,
	dollar, funFlip, funFlipConst, dot, dot2, dot3, dot4, dot5, dot6
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 関数を理解することがHaskellを理解する鍵である", \t -> do
	text t "* Haskellでは高階関数を多用する", \t -> do
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
	itext t 1 "(.) ord toLower c <-- ord (tolower c)", \t -> do
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

-- TODO
--
-- lowerOrdを引数2個の形で定義する
-- lowerOrdを試してみる
--   |
--   V

dotX :: Page
dotX = [\t -> do
	writeTopTitle t "(.)"
	text t "", \t -> do
	text t "* この関数に引数を2つだけしか指定しない場合", \t -> do
	itext t 1 "(.) f g", \t -> do
	text t "* ひとつ引数を取り", \t -> do
	itext t 1 "その引数にgとfをこの順に適用する関数となる", \t -> do
	text t "* これを関数合成と呼ぶ"
 ]
