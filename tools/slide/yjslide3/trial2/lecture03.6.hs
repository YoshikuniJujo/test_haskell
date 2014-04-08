import Lecture

subtitle :: String
subtitle = "トライアル 第3.6回 いろいろな関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	funId, funId2, funId3, funConst, funConst2, funConst3,
	funConstId, funConstId2
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

{-
	text t "* この関数も使い道がなさそうだが、そうでもない", \t -> do
	text t "* 前に見たdevideは第1引数が0のときは", \t -> do
	itext t 1 "第2引数をそのまま返していた", \t -> do
	text t "* 「第1引数が0のときは0を返す」としたい場合は", \t -> do
	itext t 1 "devide' :: Double -> Double -> Double"
 ]
 -}
