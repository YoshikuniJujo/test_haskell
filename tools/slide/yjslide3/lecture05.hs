import Control.Applicative
import Control.Monad
import System.Random
import System.IO.Unsafe
import Data.IORef

import Lecture

subtitle :: String
subtitle = "第5回 演習(1日目)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	montecarlo, getPiAlgorithm,
	syntax, functions, exponentiation, smallerEqual, aboutFromIntegral,
	aboutFstSnd, aboutFstSnd2, aboutTail, aboutTake, aboutLength,
	aboutCycle, aboutZip,
	aboutRandom, aboutRandom2, aboutRandom3,
	aboutInCircle, aboutInCircle2, aboutInCircle3, aboutInCircle4,
	aboutInCircle5, aboutInCircle6,
	randomPoints, randomPoints2, randomPoints3, randomPoints4, randomPoints5,
	randomPoints6, randomPoints6_5, randomPoints6_6, randomPoints7,
	randomPoints8, randomPoints9, randomPoints10,
	randomPoints11, randomPoints11_5, randomPoints11_6, randomPoints12,
	randomPoints13, randomPoints14,
	randomPoints15, randomPoints16,
	aboutGetPi, aboutGetPi2, aboutGetPi3, aboutGetPi4, aboutGetPi5,
	aboutResult,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今日は関数、型、タプル、リストについて学んだ", \t -> do
	text t "* タプルのところではパターンマッチについても学んだ", \t -> do
	text t "* 今の知識でできる演習問題を見ていくことにしよう", \t -> do
	text t "* モンテカルロ法で円周率を求める方法を見ていこう", \t -> do
	text t "* 必要となる関数を演習問題として出していこう"
 ]

montecarlo :: Page
montecarlo = [\t -> do
	writeTopTitle t "モンテカルロ法"
	text t "", \t -> do
	text t "* モンテカルロとは?", \t -> do
	itext t 1 "- カジノで有名なモナコ公国の1地区", \t -> do
	text t "* モンテカルロ法とは?", \t -> do
	itext t 1 "- 乱数を使うことで「決まった時間内」に", \t -> do
	itext t 1 "- 「正しい可能性の高い結果」が得られる", \t -> do
	text t "* 本当に正しいかどうかはわからない", \t -> do
	itext t 1 "- 一種の賭けである", \t -> do
	itext t 1 "- だから「モンテカルロ」法"
 ]

randomXY :: [(Double, Double)]
randomXY = unsafePerformIO $ replicateM 300 $ do
	x <- randomRIO (-1, 1)
	y <- randomRIO (-1, 1)
	return (x, y)

inPoints :: [(Int, Int)]
inPoints = flip zip [1 ..] $ tail $
	scanl (\ps (x, y) -> if inCircle x y then ps + 1 else ps) 0 randomXY

getPiAlgorithm :: Page
getPiAlgorithm = [\t -> do
	writeTopTitle t "アルゴリズム"
	text t "", \t -> do
	text t "* 円周率は半径1の円の面積に等しい", \t -> do
	text t "* 1辺が2の正方形のなかに半径1の円を書く", \t -> do
	text t "* ランダムに点を打つ", \t -> do
	text t "* 以下の値が円の面積に近づいていくと予測できる", \t -> do
	itext t 1"円の中にある点の数 / 全体の点の数 * 4", \t -> do
	drawRect2 t 150 220 120 120
	forwardRt t 60
	circleRt t 60
	t' <- mkHideTurtle (field t) 300 300
	writeRt t' "in    ="
	forwardRt t' 60
	t'' <- mkHideTurtle (field t) 300 320
	writeRt t'' "total ="
	forwardRt t'' 60
	t''' <- mkHideTurtle (field t) 300 340
	writeRt t''' "pi    ="
	forwardRt t''' 60
	writeRt t' $ show (0 :: Int)
	writeRt t'' $ show (0 :: Int)
	writeRt t''' $ show (0 :: Double)
--	replicateM_ 300 $ randomDot t 150 220 120 120
	forM_ (zip randomXY inPoints) $ \((x, y), (i, a)) -> do
		if inCircle x y then pencolor t "blue" else pencolor t "red"
		dotRt t (210 + 60 * x) (280 + 60 * y)
--		print i
--		print a
		waitTurtle t
		undo t'
		writeRt t' $ show i
		undo t''
		writeRt t'' $ show a
		undo t'''
		writeRt t''' $ take 5 $
			show (fromIntegral i / fromIntegral a * 4 :: Double)
	writeIORef killTurtles [t', t'', t''']
 ]

killTurtles :: IORef [Turtle]
killTurtles = unsafePerformIO $ newIORef []

mkHideTurtle :: Field -> Double -> Double -> IO Turtle
mkHideTurtle f x y = do
	t <- newTurtle f
	hideturtle t
	penup t
	rtGoto t x y
	return t

showDouble :: Int -> Double -> String
showDouble n d
	| d < 0 = take n $ show d
	| otherwise = ' ' : take (n - 1) (show d)

inCircle :: Double -> Double -> Bool
inCircle x y = x ^ (2 :: Int) + y ^ (2 :: Int) <= 1

syntax :: Page
syntax = [\t -> do
	readIORef killTurtles >>= mapM_ killTurtle
	pencolor t "black"
	writeTopTitle t "追加の構文"
	text t "", \t -> do
	text t "* 演習で使う追加の構文を見てみよう", \t -> do
	text t "* import [モジュール名] ([識別子1], [識別子2] ...)", \t -> do
	itext t 1 "- 指定したモジュールから指定した関数を取り込む", \t -> do
	text t "* 関数定義中のwhere", \t -> do
	itext t 1 "- where以下に関数ローカルな環境を作る", \t -> do
	itext t 1 "fun x = y + z", \t -> do
	itext t 2 "where"
	preLine t
	itext t 3 "y = x * 2", \t -> do
	itext t 3 "z = 1 / x", \t -> do
	text t "* (値 :: 型)という形", \t -> do
	itext t 1 "- 値にオプショナルな型指定をつけることができる", \t -> do
	itext t 1 "- (3 :: Int) + 8など"
 ]

functions :: Page
functions = [\t -> do
	writeTopTitle t "追加の関数"
	text t "", \t -> do
	text t "* 以下のまだ説明していない関数を使う", \t -> do
	itext t 1 "(^), (<=), fromIntegral, fst, snd, tail,"
	itext t 1 "take, length, cycle, zip, randomRs, mkStdGen", \t -> do
	text t "* ひとつずつ説明していこう"
 ]

ex1int1, ex1int2, ex1int3 :: Int
[ex1int1, ex1int2, ex1int3] = unsafePerformIO $ replicateM 3 $ randomRIO (2, 10)

ex1double1 :: Double
ex1double1 = unsafePerformIO $ ((/ 10) . fromIntegral) <$> randomRIO (11 :: Int, 99)

exponentiation :: Page
exponentiation = [\t -> do
	writeTopTitle t "べき乗"
	text t "", \t -> do
	text t "* (^)はべき乗を表す関数(演算子)", \t -> do
	text t "* x ^ nでxのn乗を表す", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "% cd ~/lectures/lecture05", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 $ "Prelude> " ++ show ex1int1 ++ " ^ " ++ show ex1int2, \t -> do
	itext t 1 $ show $ ex1int1 ^ ex1int2, \t -> do
	itext t 1 $ "Prelude> " ++ show ex1double1 ++ " ^ " ++ show ex1int3, \t -> do
	itext t 1 $ show $ ex1double1 ^ ex1int3
 ]

se1int1, se1int2, se1int3, se1int4, se1int5 :: Int
[se1int1, se1int2, se1int3, se1int4, se1int5] = unsafePerformIO $ mapM randomRIO
	[(2, 12), (13, 20), (13, 20), (2, 12), (2, 20)]

smallerEqual :: Page
smallerEqual = [\t -> do
	writeTopTitle t "小なりイコール"
	text t "", \t -> do
	text t "* (<=)は「小なりイコール」を表す関数(演算子)", \t -> do
	text t "* x <= yは", \t -> do
	itext t 1 "- xがyと等しいかまたは小さいときTrueを返し", \t -> do
	itext t 1 "- xがyより大きいときにFalseを返す", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> " ++ show se1int1 ++ " <= " ++ show se1int2, \t -> do
	itext t 1 $ show $ se1int1 <= se1int2, \t -> do
	itext t 1 $ "Prelude> " ++ show se1int3 ++ " <= " ++ show se1int4, \t -> do
	itext t 1 $ show $ se1int3 <= se1int4, \t -> do
	itext t 1 $ "Prelude> " ++ show se1int5 ++ " <= " ++ show se1int5, \t -> do
	itext t 1 $ show $ se1int5 <= se1int5
 ]

fi1int1, fi1int2 :: Int
[fi1int1, fi1int2] = unsafePerformIO $ mapM randomRIO [(2, 10), (11, 20)]

aboutFromIntegral :: Page
aboutFromIntegral = [\t -> do
	writeTopTitle t "fromIntegral"
	text t "", \t -> do
	text t "* fromIntegralは整数を他の型の数に変換する関数", \t -> do
	text t "* fromIntegral xで整数xを他の数値型に変換できる", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> fromIntegral (" ++ show fi1int1 ++ " :: Int) :: Double", \t -> do
	itext t 1 $ show $ (fromIntegral (fi1int1 :: Int) :: Double), \t -> do
	itext t 1 $ "Prelude> fromIntegral (" ++ show fi1int2 ++ " :: Int) :: Double", \t -> do
	itext t 1 $ show $ (fromIntegral (fi1int2 :: Int) :: Double)
 ]

aboutFstSnd :: Page
aboutFstSnd = [\t -> do
	writeTopTitle t "2要素タプルからの取り出し"
	text t "", \t -> do
	text t "* タプルからの要素の取り出しにはパターンマッチが使える", \t -> do
	itext t 1 "fun (x, y) = ...", \t -> do
	text t "* それ以外に、2要素タプルには要素を取り出す関数がある", \t -> do
	itext t 1 "- fst: タプルの一番目の要素を取り出す", \t -> do
	itext t 1 "- snd: タプルの二番目の要素を取り出す", \t -> do
	text t "* 以下のように定義される", \t -> do
	itext t 1 "fst :: (a, b) -> a", \t -> do
	itext t 1 "fst (x, _) = x", \t -> do
	itext t 1 "snd :: (a, b) -> b", \t -> do
	itext t 1 "snd (_, y) = y"
 ]

fs2int1 :: Int
fs2int1 = unsafePerformIO $ randomRIO (1, 10)

fs2char1 :: Char
fs2char1 = unsafePerformIO $ randomRIO ('a', 'z')

fs2tuple1 :: (Int, Char)
fs2tuple1 = (fs2int1, fs2char1)

aboutFstSnd2 :: Page
aboutFstSnd2 = [\t -> do
	writeTopTitle t "2要素タプルからの取り出し"
	text t "", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> fst " ++ show fs2tuple1, \t -> do
	itext t 1 $ show $ fst fs2tuple1, \t -> do
	itext t 1 $ "Prelude> snd " ++ show fs2tuple1, \t -> do
	itext t 1 $ show $ snd fs2tuple1
 ]

tl1lst1 :: [Int]
tl1lst1 = unsafePerformIO $ do
	sg <- newStdGen
	return $ take 10 $ randomRs (0, 20) sg

tl1lst2 :: String
tl1lst2 = unsafePerformIO $ do
	sg <- newStdGen
	return $ take 10 $ randomRs ('a', 'z') sg

aboutTail :: Page
aboutTail = [\t -> do
	writeTopTitle t "tail"
	text t "", \t -> do
	text t "* tailはリストの先頭以外の要素を返す関数", \t -> do
	itext t 1 "tail :: [a] -> [a]", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> tail " ++ show tl1lst1, \t -> do
	itext t 1 $ show $ tail tl1lst1, \t -> do
	itext t 1 $ "Prelude> tail " ++ show tl1lst2, \t -> do
	itext t 1 $ show $ tail tl1lst2
 ]

tk1lst1, tk1lst2 :: [Int]
[tk1lst1, tk1lst2] = unsafePerformIO $ replicateM 2 $ do
	sg <- newStdGen
	return $ take 10 $ randomRs (0, 20) sg

tk1int1, tk1int2 :: Int
[tk1int1, tk1int2] = unsafePerformIO $ replicateM 2 $ randomRIO (2, 9)

aboutTake :: Page
aboutTake = [\t -> do
	writeTopTitle t "take"
	text t "", \t -> do
	text t "* takeはリストのはじめのn要素を取り出す関数", \t -> do
	itext t 1 "take :: Int -> [a] -> [a]", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> take " ++ show tk1int1 ++ " " ++ show tk1lst1, \t -> do
	itext t 1 $ show $ take tk1int1 tk1lst1, \t -> do
	itext t 1 $ "Prelude> take " ++ show tk1int2 ++ " " ++ show tk1lst2, \t -> do
	itext t 1 $ show $ take tk1int2 tk1lst2, \t -> do
	itext t 1 "Prelude> take 4 \"monkey\"", \t -> do
	itext t 1 $ show $ take 4 "monkey"
 ]

aboutLength :: Page
aboutLength = [\t -> do
	writeTopTitle t "length"
	text t "", \t -> do
	text t "* lengthはリストの長さを返す関数", \t -> do
	itext t 1 "length :: [a] -> Int", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "Prelude> length [3, 2, 4, 9, 1]", \t -> do
	itext t 1 $ show $ length [3 :: Int, 2, 4, 9, 1], \t -> do
	itext t 1 "Prelude> length \"Hello, world!\"", \t -> do
	itext t 1 $ show $ length "Hello, world!"
 ]

ccl1str1 :: [Int]
[ccl1str1] = unsafePerformIO $ replicateM 1 $ do
	sg <- newStdGen
	return $ take 5 $ randomRs (0, 9) sg

ccl1str2 :: String
[ccl1str2] = unsafePerformIO $ replicateM 1 $ do
	sg <- newStdGen
	return $ take 5 $ randomRs ('a', 'z') sg

aboutCycle :: Page
aboutCycle = [\t -> do
	writeTopTitle t "cycle"
	text t "", \t -> do
	text t "* cycleはリストを無限にくりかえす関数", \t -> do
	itext t 1 "cycle :: [a] -> [a]", \t -> do
	text t "* ためすときはtakeと組み合わせると良い", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> take 20 $ cycle " ++ show ccl1str1, \t -> do
	itext t 1 $ show $ take 20 $ cycle ccl1str1, \t -> do
	itext t 1 $ "Prelude> take 30 $ cycle " ++ show ccl1str2, \t -> do
	itext t 1 $ show $ take 30 $ cycle ccl1str2
 ]

zp1lst1, zp1lst4 :: [Int]
[zp1lst1, zp1lst4] = unsafePerformIO $ forM [5, 5] $ \l -> do
	sg <- newStdGen
	return $ take l $ randomRs (0, 9) sg

zp1lst2, zp1lst3 :: String
[zp1lst2, zp1lst3] = unsafePerformIO $ forM [5, 7] $ \l -> do
	sg <- newStdGen
	return $ take l $ randomRs ('a', 'z') sg

aboutZip :: Page
aboutZip = [\t -> do
	writeTopTitle t "zip"
	text t "", \t -> do
	text t "* 2つのリストをタプルのリストに合成する関数", \t -> do
	itext t 1 "zip :: [a] -> [b] -> [(a, b)]", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "Prelude> zip " ++ show zp1lst1 ++ " " ++ show zp1lst2, \t -> do
	itext t 1 $ show $ zip zp1lst1 zp1lst2, \t -> do
	itext t 1 $ "Prelude> zip " ++ show zp1lst3 ++ " " ++ show zp1lst4, \t -> do
	itext t 1 $ show $ zip zp1lst3 zp1lst4
 ]

aboutRandom :: Page
aboutRandom = [\t -> do
	writeTopTitle t "ランダム"
	text t "", \t -> do
	text t "* モンテカルロ法を使うためにはランダムな数列が必要", \t -> do
	text t "* System.RandomモジュールのrandomRsを使おう", \t -> do
	text t "* randomRsの型を以下のように考える", \t -> do
	itext t 1 "randomRs :: (Double, Double) -> StdGen -> [Double]", \t -> do
	text t "* 第1引数はタプルで、ランダム値の(下限, 上限)となる", \t -> do
	text t "* 第2引数のStdGenは見たことのない型だ", \t -> do
	text t "* ランダム関数には初期値が必要となり", \t -> do
	itext t 1 "これをランダムの種と呼んだりする", \t -> do
	text t "* 整数(Int)からStdGenへの変換関数が用意されている", \t -> do
	itext t 1 "mkStdGen :: Int -> StdGen"
 ]

rd2int1 :: Int
rd2int1 = unsafePerformIO $ randomRIO (0, 9999)

aboutRandom2 :: Page
aboutRandom2 = [\t -> do
	writeTopTitle t "ランダム"
	text t "", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "Prelude> :m System.Random", \t -> do
	itext t 1 $ "Prelude System.Random> mkStdGen " ++ show rd2int1, \t -> do
	itext t 1 $ show $ mkStdGen rd2int1, \t -> do
	itext t 1 $ "Prelude System.Random> :t it", \t -> do
	itext t 2 "- :tで与えられた値の型を表示する", \t -> do
	itext t 2 "- 直前で評価した値は変数itに保存されている", \t -> do
	itext t 1 "it :: StdGen", \t -> do
	text t "* mkStdGenでStdGenの値が得られることがわかった"
 ]

rd3int1, rd3int2 :: Int
[rd3int1, rd3int2] = unsafePerformIO $ replicateM 2 $ randomRIO (0, 99)

aboutRandom3 :: Page
aboutRandom3 = [\t -> do
	writeTopTitle t "ランダム"
	text t "", \t -> do
	itext t (-1.2) $ "Prelude System.Random> take 3 $ randomRs (1.0, 3) $ "
		++ "mkStdGen " ++ show rd3int1, \t -> do
--	itext t 7 $ "mkStdGen " ++ show rd3int1, \t -> do
	itext t (-1.2) $ show $ take 3 $ randomRs (1 :: Double, 3) $ mkStdGen rd3int1, \t -> do
	itext t (-1.2) $ "Prelude System.Random> take 3 $ randomRs (1.0, 3) $ "
		++ "mkStdGen " ++ show rd3int2, \t -> do
	itext t (-1.2) $ show $ take 3 $ randomRs (1 :: Double, 3) $ mkStdGen rd3int2, \t -> do
	itext t (-1.2) $ "Prelude System.Random> take 3 $ randomRs (1.0, 3) $ "
		++ "mkStdGen " ++ show rd3int2, \t -> do
	itext t (-1.2) $ show $ take 3 $ randomRs (1 :: Double, 3) $ mkStdGen rd3int2, \t -> do
	text t "* 別の種には別の乱数列", \t -> do
	text t "* 同じ種には同じ乱数列", \t -> do
	text t "* Haskellの関数は同じ引数には同じ値を返す", \t -> do
	arrowIText t 1 "これを参照透過性と呼ぶ"
 ]

-- (^), (<=), fromIntegral, fst, snd, take, cycle, zip, randomRs, mkStdGen

aboutInCircle :: Page
aboutInCircle = [\t -> do
	writeTopTitle t "円のなか"
	text t "", \t -> do
	text t "* 編集用ターミナルで", \t -> do
	itext t 1 "% cd ~/lectures/lecture05", \t -> do
	itext t 1 "% nano -w montePi.hs", \t -> do
	text t "* 対話環境用ターミナルで", \t -> do
	itext t 1 "% ghci montePi.hs", \t -> do
	itext t 1 "*Main>", \t -> do
	text t "* 点(x, y)が円のなかにあることを検査する関数inCircle", \t -> do
	text t "* ここでは中心(0, 0)の半径1の円とする", \t -> do
	text t "* 演習1. inCircleの型を決めよう(1分)", \t -> do
	text t "* 演習2. inCircleの中身を作ろう(2分)"
 ]

aboutInCircle2 :: Page
aboutInCircle2 = [\t -> do
	writeTopTitle t "円のなか"
	text t "", \t -> do
	text t "* 型を決めるときの考えかた", \t -> do
	itext t 1 "- 何が欲しいのか", \t -> do
	itext t 1 "- 何が必要か", \t -> do
	text t "* 欲しいものは円のなかに「あるかどうか」", \t -> do
	arrowIText t 1 "True or False", \t -> do
	arrowIText t 1 "Bool型", \t -> do
	text t "* 必要なのは点のx座標とy座標", \t -> do
	arrowIText t 1 "Double型の値をふたつ", \t -> do
	text t "* よってこうなる", \t -> do
	itext t 1 "inCircle :: Double -> Double -> Bool"
 ]

aboutInCircle3 :: Page
aboutInCircle3 = [\t -> do
	writeTopTitle t "円のなか"
	text t "", \t -> do
	text t "* 別解: 以下の型でも正解とする", \t -> do
	itext t 1 "inCircle :: (Double, Double) -> Bool", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* 違う答えになったという方は是非教えてもらいたい", \t -> do
	text t "* 何が違うのか検討してみよう", \t -> do
	text t "* 「まちがえ」は進歩の原動力である"
	text t "", \t -> do
	text t "* ここではひとつめの解を使うので以下を書き込んでおこう", \t -> do
	itext t 1 "inCircle :: Double -> Double -> Bool"
 ]

aboutInCircle4 :: Page
aboutInCircle4 = [\t -> do
	writeTopTitle t "円のなか"
	text t "", \t -> do
	text t "* inCircleの中身について考えてみよう", \t -> do
	text t "* 中心(0, 0)で半径1の円の内側にある", \t -> do
	arrowIText t 1 "(0, 0)からの距離が1以下", \t -> do
	arrowIText t 1 "(0, 0)からの距離の2乗が1以下", \t -> do
	text t "* (0, 0)から(x, y)までの距離の2乗は", \t -> do
	itext t 1 "x ^ 2 + y ^ 2", \t -> do
	text t "* これが1以下なら良いので", \t -> do
	itext t 1 "x ^ 2 + y ^ 2 <= 1", \t -> do
	text t "* よってこうなる", \t -> do
	itext t 1 "inCircle x y = x ^ 2 + y ^ 2 <= 1"
 ]

aboutInCircle5 :: Page
aboutInCircle5 = [\t -> do
	writeTopTitle t "円のなか"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* こんなふうにまちがえた、という例があると非常に良い", \t -> do
	text t "* もしあればぜひ教えてもらいたい"
	text t "", \t -> do
	text t "* ひとつ迷ったところは(<)とするか(<=)とするか、だった", \t -> do
	text t "* 前者を「開いた単位円盤」と呼び", \t -> do
	text t "* 後者を「閉じた単位円盤」と呼ぶらしい", \t -> do
	text t "* 「単位円盤」というと通常は「開いた単位円盤」を指す"
 ]

aboutInCircle6 :: Page
aboutInCircle6 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 以下を書き込もう", \t -> do
	itext t 1 "inCircle x y = x ^ 2 + y ^ 2 <= 1", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> inCircle 0.5 0.5", \t -> do
	itext t 1 $ show $ inCircle 0.5 0.5, \t -> do
	itext t 1 "*Main> inCircle 0.8 (- 0.8)", \t -> do
	itext t 1 $ show $ inCircle 0.8 (- 0.8), \t -> do
	itext t 1 "*Main> inCircle 1 0", \t -> do
	itext t 1 $ show $ inCircle 1 0
 ]

randomPoints :: Page
randomPoints = [\t -> do
	writeTopTitle t "数列をペアに区切る"
	text t "", \t -> do
	text t "* (0, 0)を中心にする1辺2の正方形内のランダムな点を得たい", \t -> do
	text t "* ランダムな点を(x, y)とするとこの場合", \t -> do
	itext t 1 "xとyの値の範囲は両方とも-1から1となる", \t -> do
	text t "* xとyの値の範囲が同じなので", \t -> do
	itext t 1 "フラットなランダム列を2つずつに区切れば良い", \t -> do
	text t "* 再帰関数を使えば簡単だが", \t -> do
	itext t 1 "今の知識だけでも、多少技巧的だが、できる", \t -> do
	text t "* 数列を数のペアの列にする関数pairsを考えよう"
 ]

randomPoints2 :: Page
randomPoints2 = [\t -> do
	writeTopTitle t "数列をペアに区切る"
	text t "", \t -> do
	text t "* 以下のようなやりかたにしよう", \t -> do
	text t "* 数列dsとdsをひとつずらしたds'の要素同士をペアにする", \t -> do
	itext t 1 "ds = [a, b, c, d, e, f ...]", \t -> do
	itext t 1 "ds' = [b, c, d, e, f ...]", \t -> do
	itext t 1 "ps = [(a, b), (b, c), (c, d), (d, e), (e, f) ...]", \t -> do
	text t "* これをひとつ置きに取っていく", \t -> do
	itext t 1 "ps' = [(a, b), (c, d), (d, e) ...]", \t -> do
	text t "* ひとつ置きに取っていくやりかたを考えよう"
 ]

randomPoints3 :: Page
randomPoints3 = [\t -> do
	writeTopTitle t "列からひとつ置きに取る"
	text t "", \t -> do
	text t "* 列からひとつ置きに取るには以下のようにすれば良い", \t -> do
	text t "* True, Falseをくりかえす列とのzipをとる", \t -> do
	itext t 1 "tfs = [True, False, True, False, True, False ...]", \t -> do
	itext t 1 "lst = [a, b, c, d, e ...]", \t -> do
	itext t 1 "ps = [(True, a), (False, b), (True, c) ...]", \t -> do
	text t "* ペアのひとつめがTrueであるものだけを集める", \t -> do
	itext t 1 "ps' = [(True, a), (True, c), (True, e) ...]", \t -> do
	text t "* ペアのふたつめをとる", \t -> do
	itext t 1 "lst' = [a, c, e ...]", \t -> do
	text t "* 演習3. True, Falseをくりかえすtfsを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

tfs :: [Bool]
tfs = cycle [True, False]

randomPoints4 :: Page
randomPoints4 = [\t -> do
	writeTopTitle t "[True, False, True, False ...]"
	text t "", \t -> do
	text t "* できただろうか", \t -> do
	text t "* これはさっき説明したcycleを思い出せたかどうか、だ", \t -> do
	itext t 1 "tfs :: [Bool]", \t -> do
	itext t 1 "tfs = cycle [True, False]", \t -> do
	text t "* montePi.hsに追加しよう", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> take 7 tfs", \t -> do
	itext t 1 $ show $ take 7 tfs
 ]

randomPoints5 :: Page
randomPoints5 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* ひとつ置きに取る戦略をもう一度くりかえす", \t -> do
	itext t 1 "1. tfsとのzipをとる(zipとは各要素同士のペア)", \t -> do
	itext t 1 "2. ペアの1番目の値でfilterする", \t -> do
	itext t 1 "3. ペアの2番目の値だけをとる", \t -> do
	text t "* これを行う関数をhopとしよう", \t -> do
	text t "* 演習4. hopの型を決めよ(1分)", \t -> do
	text t "* 演習5. hopの中身を作れ(2分)"
 ]

randomPoints6 :: Page
randomPoints6 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* hopは「何かのリスト」をとって「同じ型のリスト」を返す", \t -> do
	itext t 1 "hop :: [a] -> [a]", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* これは多相関数である"
 ]

randomPoints6_5 :: Page
randomPoints6_5 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* まずはtfsとのzipをとる", \t -> do
	itext t 1 "zip tfs lst", \t -> do
	itext t 1 "[(True, x), (False, y), (True, z), (False, w) ...]", \t -> do
	text t "* これをタプルの第1要素が真か偽かでfilterする", \t -> do
	itext t 1 "filter (\\(b, _) -> b == True) $ zip tfs lst", \t -> do
	text t "* bとb == Trueとは同値", \t -> do
	itext t 1 "- bがTrueのときb == TrueはTrue", \t -> do
	itext t 1 "- bがFalseのときb == TrueはFalse", \t -> do
	text t "* よって\\(b, _) -> b == Trueは\\(b, _) -> bとなる", \t -> do
	text t "* タプルの第一要素を取っているだけなのでfst関数と同値", \t -> do
	itext t 1 "filter fst $ zip tfs lst"
 ]

randomPoints6_6 :: Page
randomPoints6_6 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* 今の段階で手に入るもの", \t -> do
	itext t 1 "filter fst $ zip tfs lst", \t -> do
	itext t 1 "[(True, x), (True, z), (True, v) ...]", \t -> do
	text t "* このリストのすべての要素であるタプルの2番目を取る", \t -> do
	itext t 1 "map snd $ filter fst $ zip tfs lst", \t -> do
	text t "* よって関数hopは", \t -> do
	itext t 1 "hop lst = map snd $ filter fst $ zip tfs lst"
 ]

randomPoints7 :: Page
randomPoints7 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* filter fstにおける型について見てみよう", \t -> do
	itext t 1 "filter :: (a -> Bool) -> [a] -> [a]", \t -> do
	itext t 1 "fst :: (b, c) -> b", \t -> do
	text t "* (a -> Bool)と((b, c) -> b)とですりあわせが行われる", \t -> do
	itext t 1 "Bool == b", \t -> do
	itext t 1 "a == (b, c) == (Bool, c)", \t -> do
	text t "* その結果以下のような型になる", \t -> do
	itext t 1 "filter :: ((Bool, c) -> Bool) ->"
	itext t 3 "[(Bool, c)] -> [(Bool, c)]", \t -> do
	itext t 1 "fst :: (Bool, c) -> Bool"
 ]

randomPoints8 :: Page
randomPoints8 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* 解答例をもう一度見てみる", \t -> do
	itext t 1 "hop lst = map snd $ filter fst $ zip tfs lst", \t -> do
	text t "* これはlstから始まり次々に値を渡していく構造", \t -> do
	text t "* こう書き直せる", \t -> do
	itext t 1 "hop = map snd . filter fst . zip tfs", \t -> do
	text t "* hop関数は", \t -> do
	itext t 1 "- tfsとのzipをとり", \t -> do
	itext t 1 "- タプルの1番目の要素でfilterし", \t -> do
	itext t 1 "- すべてのタプルの2番目の要素をとる"
 ]

hop :: [a] -> [a]
hop = map snd . filter fst . zip tfs

randomPoints9 :: Page
randomPoints9 = [\t -> do
	writeTopTitle t "ひとつ置きに取る"
	text t "", \t -> do
	text t "* montePi.hsに書き込む", \t -> do
	itext t 1 "hop :: [a] -> [a]", \t -> do
	itext t 1 "hop = map snd . filter fst . zip tfs", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> hop [1 .. 10]", \t -> do
	itext t 1 $ show $ hop [1 :: Int .. 10]
 ]

randomPoints10 :: Page
randomPoints10 = [\t -> do
	writeTopTitle t "ペアに区切る"
	text t "", \t -> do
	text t "* ペアに区切るアルゴリズムを再掲する", \t -> do
	itext t 1 "1. リストとそれ自体をひとつずらしたリストとをzip", \t -> do
	itext t 1 "2. 結果として出来たリストの要素をひとつ置きに取る", \t -> do
	text t "* この関数をpairsとしよう", \t -> do
	text t "* 演習6. pairsの型を決めよ(1分)", \t -> do
	text t "* 演習7. pairsの中身を作れ(2分)"
 ]

pairs :: [a] -> [(a, a)]
pairs lst = hop $ zip lst $ tail lst

randomPoints11 :: Page
randomPoints11 = [\t -> do
	writeTopTitle t "ペアに区切る"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* pairsの入力は「何かのリスト」なので[a]", \t -> do
	text t "* pairsの出力は「同じ何かのペアのリスト」なので[(a, a)]", \t -> do
	arrowIText t 1 "pairs :: [a] -> [(a, a)]"
 ]

randomPoints11_5 :: Page
randomPoints11_5 = [\t -> do
	writeTopTitle t "ペアに区切る"
	text t "", \t -> do
	text t "* 以下のようなリストがあったとして", \t -> do
	itext t 1 "lst = [x, y, z, w, v, u ...]", \t -> do
	text t "* ひとつずらしたリストは", \t -> do
	itext t 1 "tail lst", \t -> do
	itext t 1 "[y, z, w, v, u ...]", \t -> do
	text t "* それともとのリストとのzipは", \t -> do
	itext t 1 "zip lst $ tail lst", \t -> do
	itext t 1 "[(x, y), (y, z), (z, w), (w, v), (v, u) ...]", \t -> do
	text t "* これをひとつ置きにとると", \t -> do
	itext t 1 "hop $ zip $ tail lst", \t -> do
	itext t 1 "[(x, y), (z, w), (v, u) ...]"
 ]

randomPoints11_6 :: Page
randomPoints11_6 = [\t -> do
	writeTopTitle t "ペアに区切る"
	text t "", \t -> do
	text t "* よって、以下のようになる", \t -> do
	itext t 1 "pairs lst = hop $ zip lst $ tail lst", \t -> do
	text t "* tail lstはひとつずれたリスト", \t -> do
	text t "* それともとのリストとのzipを取り", \t -> do
	text t "* そのリストから要素をひとつ置きに取っている"
 ]

randomPoints12 :: Page
randomPoints12 = [\t -> do
	writeTopTitle t "ペアに区切る"
	text t "", \t -> do
	text t "* 以下をmontePi.hs追加しよう", \t -> do
	itext t 1 "pairs :: [a] -> [(a, a)]", \t -> do
	itext t 1 "pairs lst = hop $ zip lst $ tail lst", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> pairs [1 .. 10]", \t -> do
	itext t 1 $ show $ pairs [1 :: Int .. 10], \t -> do
	itext t 1 "*Main> pairs \"Haskell\"", \t -> do
	itext t 1 $ show $ pairs "Haskell"
 ]

randomPoints13 :: Page
randomPoints13 = [\t -> do
	writeTopTitle t "ランダムな点"
	text t "", \t -> do
	text t "* 種を与えるとランダムな点のリストを返す関数を作ろう", \t -> do
	itext t 1 "- 点の範囲はx, yともに-1から1のあいだとする", \t -> do
	itext t 1 "- 中心が(0, 0)の一辺が2の正方形内ということ", \t -> do
	text t "* この関数をpointsという名前にしよう", \t -> do
	text t "* ランダムの種の型はStdGenとする", \t -> do
	text t "* 与えられた範囲の乱数列を返す関数は", \t -> do
	itext t 1 "randomRs :: (Double, Double) -> StdGen -> [Double]", \t -> do
	text t "* 演習8. pointsの型を決めよ(1分)", \t -> do
	text t "* 演習9. pointsの中身を作れ(2分)"
 ]

randomPoints14 :: Page
randomPoints14 = [\t -> do
	writeTopTitle t "ランダムな点"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* pointsの入力はランダムの種なのでStdGen", \t -> do
	text t "* pointsの出力は点のリストなので[(Double, Double)]", \t -> do
	arrowIText t 1 "points :: StdGen -> [(Double, Double)]", \t -> do
	text t "* pointsはランダム列を2つずつのペアに区切ったものなので", \t -> do
	itext t 1 "points sg = pairs $ randomRs (- 1, 1) sg", \t -> do
	text t "* sgから始めて次々に関数を適用していく構造なので", \t -> do
	itext t 1 "points = pairs . randomRs (- 1, 1)"
 ]

points :: StdGen -> [(Double, Double)]
points = pairs . randomRs (- 1, 1)

randomPoints15 :: Page
randomPoints15 = [\t -> do
	writeTopTitle t "ランダムな点"
	text t "", \t -> do
	text t "* montePi.hsに書き込もう", \t -> do
	itext t 1 "points :: StgGen -> [(Double, Double)]", \t -> do
	itext t 1 "points = pairs . randomRs (- 1, 1)", \t -> do
	text t "* System.RandomモジュールのrandomRsを使っているので", \t -> do
	text t "* ファイルの先頭に以下を追加しよう", \t -> do
	itext t 1 "import System.Random (randomRs, mkStdGen)", \t -> do
	itext t 2 "- 対話環境から使うのでmkStdGenもついでに"
 ]

randomPoints16 :: Page
randomPoints16 = [\t -> do
	writeTopTitle t "ランダムな点"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> take 3 $ points $ mkStdGen 8", \t -> do
	let ps = points $ mkStdGen 8
	itext t 1 $ "[" ++ show (ps !! 0) ++ ","
	itext t 1.5 $ show (ps !! 1) ++ ","
	itext t 1.5 $ show (ps !! 2) ++ "]", \t -> do
	itext t 1 "*Main> take 3 $ points $ mkStdGen 100", \t -> do
	let ps = points $ mkStdGen 100
	itext t 1 $ "[" ++ show (ps !! 0) ++ ","
	itext t 1.5 $ show (ps !! 1) ++ ","
	itext t 1.5 $ show (ps !! 2) ++ "]"
 ]

aboutGetPi :: Page
aboutGetPi = [\t -> do
	writeTopTitle t "πの値を求める"
	text t "", \t -> do
	text t "* モンテカルロ法でπの値を求めよう", \t -> do
	text t "* 半径1の円の面積を求めるということ", \t -> do
	text t "* getPi関数を作ろう", \t -> do
	text t "* getPiは点を打つ数とランダムの種を入力とし", \t -> do
	text t "* πの近似値を出力とする", \t -> do
	text t "* n個のランダムな点から円内の点を選び", \t -> do
	text t "* その要素数をnでわったものを求める", \t -> do
	text t "* それに正方形の面積である4をかける", \t -> do
	text t "* 演習10. getPiの型を決めよ", \t -> do
	itext t 1 "(1分)"
 ]

aboutGetPi2 :: Page
aboutGetPi2 = [\t -> do
	writeTopTitle t "πの値を求める"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	itext t 1 "getPi :: Int -> StdGen -> Double", \t -> do
	text t "* 演習11. 以下の[ (a) ]に入る式を求めよ", \t -> do
	itext t 0 "getPi n sg ="
	itext t 1 "4 * fromIntegral (length inps) / fromIntegral n"
	itext t 1 "where inps = [ (a) ]", \t -> do
	itext t 2 "-- lengthの返り値やnはIntなので", \t -> do
	itext t 2 "-- fromIntegralによってDoubleに変換している", \t -> do
	text t "* ヒント1: inpsはn個の点のうち円のなかにある点", \t -> do
	itext t 1 "(1分)"
 ]

aboutGetPi3 :: Page
aboutGetPi3 = [\t -> do
	writeTopTitle t "πの値を求める"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* ヒント2: 円のなかかどうかを表す関数は", \t -> do
	itext t 1 "inCircle :: Double -> Double -> Bool", \t -> do
	text t "* ヒント3: 非カリー化を使う", \t -> do
	itext t 1 "uncurry :: (a -> b -> c) -> (a, b) -> c", \t -> do
	text t "* ヒント4: n個のランダムな点は以下で表せる", \t -> do
	itext t 1 "take n $ points sg", \t -> do
	text t "* 演習11'. 種sg、ランダムな点の個数nが与えられている"
	itext t 1 "その点のうち単位円内にある点のリストを求めよ", \t -> do
	itext t 1 "(1分)"
 ]

aboutGetPi4 :: Page
aboutGetPi4 = [\t -> do
	writeTopTitle t "πの値を求める"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	itext t 1 "filter (uncurry inCircle) $ take n $ points sg", \t -> do
	text t "* getPi関数をmontePi.hsに書き込もう", \t -> do
	itext t 0 "getPi :: Int -> StdGen -> Double", \t -> do
	itext t 0 "getPi n sg ="
	itext t 0.5 "4 * fromIntegral (length inps) / fromIntegral n"
	itext t 0.5 "where"
	itext t 0.5 "inps = filter (uncurry inCircle) $ take n $ points sg", \t -> do
	text t "* できた!", \t -> do
	text t "* モンテカルロ法で円周率を求める関数が作れた"
 ]

getPi :: Int -> StdGen -> Double
getPi n sg = 4 * fromIntegral (length inps) / fromIntegral n
	where inps = filter (uncurry inCircle) $ take n $ points sg

gp5int1, gp5int2, gp5int3, gp5int4 :: Int
[gp5int1, gp5int2, gp5int3, gp5int4] = unsafePerformIO $ replicateM 4 randomIO

aboutGetPi5 :: Page
aboutGetPi5 = [\t -> do
	writeTopTitle t "πの値を求める"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> getPi 100 $ mkStdGen " ++ showsPrec 7 gp5int1 "", \t -> do
	itext t 1 $ show $ getPi 100 $ mkStdGen gp5int1, \t -> do
	itext t 1 $ "*Main> getPi 1000 $ mkStdGen " ++ showsPrec 7 gp5int2 "", \t -> do
	itext t 1 $ show $ getPi 1000 $ mkStdGen gp5int2, \t -> do
	itext t 1 $ "*Main> getPi 10000 $ mkStdGen " ++ showsPrec 7 gp5int3 "", \t -> do
	itext t 1 $ show $ getPi 10000 $ mkStdGen gp5int3, \t -> do
	itext t 1 $ "*Main> getPi 100000 $ mkStdGen " ++ showsPrec 7 gp5int4 "", \t -> do
	itext t 1 $ show $ getPi 100000 $ mkStdGen gp5int4
 ]

aboutResult :: Page
aboutResult = [\t -> do
	writeTopTitle t "結果について"
	text t "", \t -> do
	text t "* ランダムの種の違いによって値は大きく変化する", \t -> do
	text t "* 結果は「正しい可能性が高い」", \t -> do
	text t "* 試行を増やせば「正しい可能性」は高まる、が", \t -> do
	text t "* 必ずしも値に近づいていくとは限らない", \t -> do
	text t "* 円周率には近似値を求める効率的なアルゴリズムがあるので", \t -> do
	itext t 1 "モンテカルロ法は使う必要がない", \t -> do
	text t "* 正しい値を求めるアルゴリズムの計算量が膨大であるときに", \t -> do
	itext t 1 "モンテカルロ法は役に立つ"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* モンテカルロ法で円周率を求める関数を作った", \t -> do
	text t "* そこで使われる関数を定義していくことで", \t -> do
	itext t 1 "関数を作る/使う練習をした", \t -> do
	text t "* リストを扱う関数の使いかたも学んだ", \t -> do
	text t "* 直接、再帰的定義を使わなくても", \t -> do
	itext t 1 "リストを扱う関数でかなりのことができることを学んだ"
 ]
