import Control.Applicative
import Control.Monad
import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第11回 代数的データ型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	enumerate, enumerate2, enumerate3,
	boolAsEnumerate, intAsEnumerate,
	abstract,
	coordinate, coordinate2, coordinate3, coordinate4, coordinate5,
	coordinate6, coordinate7, coordinate8, coordinate9, coordinate10,
	coordinate11, coordinate12, coordinate13, coordinate14, coordinate15,
	adt, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今までは用意された型に対する演算を扱ってきた", \t -> do
	text t "* Haskellでは自分で型を作ることができる", \t -> do
	text t "* 型の作りかたには以下の方法が考えられる", \t -> do
	itext t 1 "- 値を列挙する", \t -> do
	itext t 1 "- 既存の型を組み合わせる", \t -> do
	text t "* Haskellでは両者が無理なく1つの枠組みにまとまっている", \t -> do
	text t "* すくなくとも意味論的には整数や文字型は", \t -> do
	itext t 1 "値を列挙することによって作られた型と考えられる"
 ]

enumerate :: Page
enumerate = [\t -> do
	writeTopTitle t "列挙する"
	text t "", \t -> do
	text t "* 値を列挙することで型を作ることができる", \t -> do
	text t "* 「値」は大文字ではじまる識別子", \t -> do
	itext t 1 "data Friend = Takashi | Kazuya | Keiko", \t -> do
	itext t 1 "data Gender = Man | Woman", \t -> do
	text t "* 3人の友達を表現する型Friendと", \t -> do
	text t "* 性別を表現する型Genderとを作った", \t -> do
	text t "* これらの型を扱う関数genderを作ってみる", \t -> do
	itext t 1 "gender :: Friend -> Gender", \t -> do
	itext t 1 "gender Takashi = Man", \t -> do
	itext t 1 "gender Kazuya = Man", \t -> do
	itext t 1 "gender Keiko = Woman"
 ]

enumerate2 :: Page
enumerate2 = [\t -> do
	writeTopTitle t "列挙する"
	text t "", \t -> do
	text t "* ここまでの定義をlectures/lecture11/friend.hsに書きこむ", \t -> do
	itext t 1 "data Friend = Takashi | Kazuya | Keiko"
	itext t 1 "data Gender = Man | Woman"
	itext t 1 ""
	itext t 1 "gender :: Friend -> Gender"
	itext t 1 "gender Takashi = Man"
	itext t 1 "gender Kazuya = Man"
	itext t 1 "gender Keiko = Woman"
 ]

data Friend = Takashi | Kazuya | Keiko
data Gender = Man | Woman

gender :: Friend -> Gender
gender Takashi = Man
gender Kazuya = Man
gender Keiko = Woman

genderCheck :: Friend -> String
genderCheck f = case gender f of
	Man -> "He is a man."
	Woman -> "She is a woman."

enumerate3 :: Page
enumerate3 = [\t -> do
	writeTopTitle t "列挙する"
	text t "", \t -> do
	text t "* 男女を判定しメッセージを作成する", \t -> do
	itext t 1 "genderCheck :: Friend -> String"
	itext t 1 "genderCheck f = case gender f of"
	itext t 2 "Man -> \"He is a man.\""
	itext t 2 "Woman -> \"She is a woman.\"", \t -> do
	text t "* 上記の定義もfriend.hsに書き込み", \t -> do
	itext t 1 "% ghci friend.hs", \t -> do
	itext t 1 "*Main> genderCheck Takashi", \t -> do
	itext t 1 $ show $ genderCheck Takashi, \t -> do
	itext t 1 "*Main> genderCheck Keiko", \t -> do
	itext t 1 $ show $ genderCheck Keiko
 ]

boolAsEnumerate :: Page
boolAsEnumerate = [\t -> do
	writeTopTitle t "Bool型の定義"
	text t "", \t -> do
	text t "* Haskellでははじめから使える型としてBool型があるが", \t -> do
	text t "* これは以下のように定義できる", \t -> do
	itext t 1 "data Bool = False | True", \t -> do
	text t "* Bool型は構文ではなく"
	itext t 1 "単なるライブラリとして用意されている"
 ]

intAsEnumerate :: Page
intAsEnumerate = [\t -> do
	writeTopTitle t "整数型と文字型"
	text t "", \t -> do
	text t "* 今まで使ってきたIntも数の列挙と考えられる", \t -> do
	itext t 1 "data Int ="
	itext t 2 "-2147483648 | -2147483647 | ... | -2 | -1 |"
	itext t 2 "0 | 1 | 2 | ... | 2147483646 | 2147483647", \t -> do
	text t "* Char型は文字の列挙", \t -> do
	itext t 1 "data Char ="
	itext t 2 "'\\0' | '\\1' | ... | 'A' | 'B' | ... |"
	itext t 2 "'a' | 'b' | ..."
 ]

abstract :: Page
abstract = [\t -> do
	writeTopTitle t "既存の型を組み合わせる"
	text t "", \t -> do
	text t "* 既存の型を組み合わせて新しい型を作る例を見ていく", \t -> do
	text t "* 座標上の点についての例を見る", \t -> do
	text t "* まずはタプルによる実装を試す", \t -> do
	text t "* その実装の問題点を示し、新しい型を作る方法と比較する", \t -> do
	text t "* それでは見ていこう"
 ]

coordinate :: Page
coordinate = [\t -> do
	writeTopTitle t "直交座標"
	text t "", \t -> do
	text t "* 直交座標上の位置(x, y)を以下のようにタプルで表現する", \t -> do
	itext t 1 "type Rect = (Double, Double)", \t -> do
	text t "* これをベクトルと見てn倍する関数を考える", \t -> do
	text t "* n = 3の例を以下に示す", \t -> do
	rtGoto t 100 350
	setheading t 0
	pendown t
	forwardRt t 250
	penup t
	rtGoto t 150 370
	setheading t 90
	pendown t
	forwardRt t 170
	penup t
	rtGoto t 150 350
	setheading t 30
	pendown t
	forwardRt t 70

	penup t
	setheading t 0
	forwardRt t 10
	setheading t (- 90)
	forwardRt t 10
	writeRt t "(x, y)"
	setheading t 0
	backwardRt t 10
	setheading t (- 90)
	backwardRt t 10
	pendown t

	setheading t 30
	left t 90
	backwardRt t 5
	forwardRt t 10
	backwardRt t 5
	right t 90
	forwardRt t 70
	left t 90
	backwardRt t 5
	forwardRt t 10
	backwardRt t 5
	right t 90
	forwardRt t 70
	left t 90
	backwardRt t 5
	forwardRt t 10
	backwardRt t 5

	penup t
	setheading t 0
	forwardRt t 10
	setheading t (- 90)
	forwardRt t 10
	writeRt t "(x', y')"
	setheading t 0
	backwardRt t 10
	setheading t (- 90)
	backwardRt t 10
{-
	pendown t
	penup t
	right t 90
	setheading t 0
	forwardRt t 50
	writeRt t "n = 3"
	-}
 ]

crd2x, crd2y, crd2n :: Double
[crd2x, crd2y, crd2n] = unsafePerformIO $ replicateM 3 $ fromIntegral <$>
	randomRIO (2 :: Int, 10)

type Rect = (Double, Double)
mulRect :: Rect -> Double -> Rect
mulRect (x, y) n = (x * n, y * n)

coordinate2 :: Page
coordinate2 = [\t -> do
	writeTopTitle t "直交座標"
	text t "", \t -> do
	text t "* x, yをともにn倍すれば良いので以下のようになる", \t -> do
	itext t 1 "mulRect :: Rect -> Double -> Rect", \t -> do
	itext t 1 "mulRect (x, y) n = (x * n, y * n)", \t -> do
	text t "* Rectの定義とともにcoordinate.hsに書き込もう", \t -> do
	itext t 1 "type Rect = (Double, Double)", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :load coordinate.hs", \t -> do
	itext t 1 $ "*Main> mulRect " ++ show (crd2x, crd2y) ++ " " ++ show crd2n, \t -> do
	itext t 1 $ show $ mulRect (crd2x, crd2y) crd2n
 ]

coordinate3 :: Page
coordinate3 = [\t -> do
	writeTopTitle t "極座標"
	text t "", \t -> do
	text t "* 極座標による位置の表現は以下のようになる", \t -> do
	itext t 1 "(原点からの距離, x軸からの角度)", \t -> do
	itext t 1 "data Pol = (Double, Double)", \t -> do
	text t "* (dist, rad)の場合を以下に示す", \t -> do
	rtGoto t 100 330
	setheading t 0
	pendown t
	forwardRt t 250
	penup t
	rtGoto t 150 350
	setheading t 90
	pendown t
	forwardRt t 150
	penup t
	rtGoto t 150 330
	setheading t 30
	pendown t
	forwardRt t 200
	penup t
	backwardRt t 120
	left t 90
	forwardRt t 20
	writeRt t "dist"
	backwardRt t 20
	right t 90
	backwardRt t 60
	pendown t
	right t 90
	replicateM_ 30 $ right t 1 >> forwardRt t 0.35
	penup t
	setheading t 30
	forwardRt t 15
	writeRt t "rad"
 ]

type Pol = (Double, Double)

mulPol :: Pol -> Double -> Pol
mulPol (dist, rad) n = (dist * n, rad)

coordinate4 :: Page
coordinate4 = [\t -> do
	writeTopTitle t "極座標"
	text t "", \t -> do
	text t "* これを前の例と同様にn倍する関数を書く", \t -> do
	text t "* 原点からの距離をn倍すれば良いので", \t -> do
	itext t 1 "mulPol :: Pol -> Double -> Pol", \t -> do
	itext t 1 "mulPol (dist, rad) n = (dist * n, rad)", \t -> do
	text t "* Polの定義とともにcoordinate.hsに書き込もう", \t -> do
	itext t 1 "type Pol = (Double, Double)", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> mulPol (8, pi / 6) 5", \t -> do
	itext t 1 $ show $ mulPol (8, pi / 6) 5
 ]

pa :: Rect
pa = (3, 8)

pb :: Pol
pb = (7, pi / 6)

coordinate5 :: Page
coordinate5 = [\t -> do
	writeTopTitle t "点を定義する"
	text t "", \t -> do
	text t "* 以下の定義をcoordinate.hsに書き込もう", \t -> do
	itext t 1 "pa :: Rect", \t -> do
	itext t 1 "pa = (3, 8)", \t -> do
	itext t 1 "pb :: Pol", \t -> do
	itext t 1 $ "pb = (7, pi / 6)", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> mulRect pa 3", \t -> do
	itext t 1 $ show $ mulRect pa 3, \t -> do
	itext t 1 "*Main> mulPol pb 2", \t -> do
	itext t 1 $ show $ mulPol pb 2
 ]

coordinate6 :: Page
coordinate6 = [\t -> do
	writeTopTitle t "おかしな変換"
	text t "", \t -> do
	text t "* ここまではうまくいっている", \t -> do
	text t "* しかし以下の例を見てみよう", \t -> do
	itext t 1 "*Main> mulRect pb 4", \t -> do
	itext t 1 $ show $ mulRect pb 4, \t -> do
	itext t 1 "*Main> mulPol pa 5", \t -> do
	itext t 1 $ show $ mulPol pa 5, \t -> do
	text t "* 極座標として定義したpbに直交座標用の関数を", \t -> do
	text t "* 直交座標として定義したpaに極座標用の関数を適用している"
 ]

coordinate7 :: Page
coordinate7 = [\t -> do
	writeTopTitle t "おかしな変換"
	text t "", \t -> do
	text t "* typeは型の「別名」を作る構文", \t -> do
	text t "* 単なる「別名」なのでそれぞれの型は区別されない", \t -> do
	text t "* RectとPolは中身はまったく同じである", \t -> do
	itext t 1 "type Rect = (Double, Double)", \t -> do
	itext t 1 "type Pol = (Double, Double)", \t -> do
	text t "* 本当ならば以下のようにしたい", \t -> do
	itext t 1 "- mulRectはPolには使えない", \t -> do
	itext t 1 "- mulPolはRectには使えない", \t -> do
	text t "* それぞれ区別されるRect, Polを作る必要がある"
 ]

coordinate8 :: Page
coordinate8 = [\t -> do
	writeTopTitle t "それぞれの座標型を作る"
	text t "", \t -> do
	text t "* 以下のようにする", \t -> do
	itext t 1 "data Rect = Rect Double Double", \t -> do
	itext t 1 "data Pol = Pol Double Double", \t -> do
	text t "* これでこの2つの型は完全に区別されるようになる", \t -> do
	text t "* 左側のRectは型構築子であり右側のRectは値構築子である", \t -> do
	text t "* ここでは同じRectを使っているが、別の名前にしても良い"
 ]

coordinate9 :: Page
coordinate9 = [\t -> do
	writeTopTitle t "直交座標用の関数"
	text t "", \t -> do
	text t "* 直交座標用の関数を作ろう", \t -> do
	itext t 1 "mulRect :: Rect -> Double -> Rect", \t -> do
	itext t 1 "mulRect (Rect x y) n = Rect (x * n) (y * n)", \t -> do
	text t "* 一行目では型構築子を使っている", \t -> do
	text t "* 二行目の左側では値構築子Rectをパターンマッチに使い", \t -> do
	itext t 1 "右側では値構築子Rectを値を構築するのに使っている", \t -> do
	text t "* これをRectの定義とともにcoordinate2.hsに書き込もう", \t -> do
	itext t 1 "data Rect = Rect Double Double deriving Show", \t -> do
	text t "* 'deriving Show'はghciで表示するために必要", \t -> do
	itext t 1 "- 詳細は「型クラス」の回で見る"
 ]

data Rect' = Rect Double Double deriving Show

mulRect' :: Rect' -> Double -> Rect'
mulRect' (Rect x y) n = Rect (x * n) (y * n)

data Pol' = Pol Double Double deriving Show

mulPol' :: Pol' -> Double -> Pol'
mulPol' (Pol dist rad) n = Pol (dist * n) rad

coordinate10 :: Page
coordinate10 = [\t -> do
	writeTopTitle t "極座標用の関数"
	text t "", \t -> do
	text t "* 極座標用の関数を作ろう", \t -> do
	itext t 1 "mulPol :: Pol -> Double -> Pol", \t -> do
	itext t 1 "mulPol (Pol dist rad) n = Pol (dist * n) rad", \t -> do
	text t "* Polの定義とともにcoordinate2.hsに書き込もう", \t -> do
	itext t 1 "data Pol = Pol Double Double deriving Show", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :load coordinate2.hs", \t -> do
	itext t 1 "*Main> mulRect (Rect 5 8) 4", \t -> do
	itext t 1 $ show $ mulRect' (Rect 5 8) 4, \t -> do
	itext t 1 "*Main> mulPol (Pol 4 (pi / 3)) 3", \t -> do
	itext t 1 $ show $ mulPol' (Pol 4 (pi / 3)) 3
 ]

coordinate11 :: Page
coordinate11 = [\t -> do
	writeTopTitle t "型のミスマッチ"
	text t "", \t -> do
	text t "* 型の合わない値を使うと", \t -> do
	itext t 1 "*Main> mulRect (Pol 4 (pi / 3)) 3", \t -> do
	itext t 1 "..."
	itext t 1 "Couldn't match expected type `Rect' with actural"
	itext t 1 "type `Pol'"
	itext t 1 "...", \t -> do
	itext t 1 "*Main> mulPol (Rect 5 8) 4", \t -> do
	itext t 1 "..."
	itext t 1 "Couldn't match expected type `Pol' with actural"
	itext t 1 "type `Rect'"
	itext t 1 "..."
 ]

coordinate12 :: Page
coordinate12 = [\t -> do
	writeTopTitle t "特定の型"
	text t "", \t -> do
	text t "* 同じ「実数のペア」でも直交座標と極座標では意味が違う", \t -> do
	text t "* data構文を使うとそれぞれ区別される新しい型が作れる", \t -> do
	text t "* 他の例として「文字列と整数のペア」を考えると", \t -> do
	text t "* その解釈としては", \t -> do
	itext t 1 "- 「名前」と「年齢」で「人」を表現する", \t -> do
	itext t 1 "- 「商品名」と「値段」で「商品」を表現する", \t -> do
	itext t 1 "- 等々", \t -> do
	text t "* ペアで構成されるものが「人」か「商品」かを区別できる"
 ]

coordinate13 :: Page
coordinate13 = [\t -> do
	writeTopTitle t "しかし点は点だ"
	text t "", \t -> do
	text t "* 「人」と「商品」は確かに違うものだ", \t -> do
	text t "* しかし直交座標で表現しても極座標で表現しても点は点だ", \t -> do
	text t "* 両者を同じ座標上にプロットすることもできる", \t -> do
	text t "* だったら同じ型にまとめられないの?", \t -> do
	itext t 1 "「できます」", \t -> do
	text t "* 以下のようにすれば良い", \t -> do
	itext t 1 "data Point"
	itext t 2 "= Rect Double Double"
	itext t 2 "| Pol Double Double", \t -> do
	text t "* Pointという集合は"
	itext t 1 "Rect Double DoubleとPol Double Doubleを含む"
 ]

coordinate14 :: Page
coordinate14 = [\t -> do
	writeTopTitle t "mulPointを定義しよう"
	text t "", \t -> do
	text t "* Pointに対するmulPoint関数を考えよう", \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "mulPoint :: Point -> Double -> Point", \t -> do
	itext t 1 "mulPoint (Rect x y) n = Rect (x * n) (y * n)", \t -> do
	itext t 1 "mulPoint (Pol dist rad) n = Pol (dist * n) rad", \t -> do
	text t "* これをPointの定義とともにcoordinate3.hsに書き込もう", \t -> do
	itext t 1 "data Point"
	itext t 2 "= Rect Double Double"
	itext t 2 "| Pol Double Double"
	itext t 2 "deriving Show"
 ]

data Point
	= Rect' Double Double
	| Pol' Double Double

instance Show Point where
	show (Rect' x y) = "Rect " ++ show x ++ " " ++ show y
	show (Pol' dist rad) = "Pol " ++ show dist ++ " " ++ show rad

mulPoint :: Point -> Double -> Point
mulPoint (Rect' x y) n = Rect' (x * n) (y * n)
mulPoint (Pol' dist rad) n = Pol' (dist * n) rad

coordinate15 :: Page
coordinate15 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :load coordinate3.hs", \t -> do
	itext t 1 "*Main> mulPoint (Rect 2 8) 4", \t -> do
	itext t 1 $ show $ mulPoint (Rect' 2 8) 4, \t -> do
	itext t 1 "*Main> mulPoint (Pol 3 (pi / 4)) 5", \t -> do
	itext t 1 $ show $ mulPoint (Pol' 3 (pi / 4)) 5, \t -> do
	text t "* 直交座標と極座標で適切な計算が行われているのがわかる"
 ]

adt :: Page
adt = [\t -> do
	writeTopTitle t "代数的データ型"
	text t "", \t -> do
	text t "* 型を作成する構文は以下のようになる", \t -> do
	itext t 1 "data [型名]"
	itext t 2 "= [値構築子1] [型1] [型2] ..."
	itext t 2 "| [値構築子2] [型1] [型2] ..."
	itext t 2 "...", \t -> do
	text t "* このように作られる型を代数的データ型と呼ぶ", \t -> do
	text t "* はじめに挙げた列挙による型は", \t -> do
	itext t 1 "- 上の定義の特殊な場合と考えられる、つまり", \t -> do
	itext t 1 "- すべての値構築子がとる引数が0個の場合"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 列挙によって型を作る例を見た", \t -> do
	text t "* タプルによって点を表現する例を見た", \t -> do
	itext t 1 "- 直交座標と極座標の混同が起こり得る", \t -> do
	text t "* 代数的データ型によって点を表現する例を見た", \t -> do
	itext t 1 "- 直交座標と極座標を区別することができた", \t -> do
	itext t 1 "- 直交座標と極座標をまとめて", \t -> do
	itext t 2 "それぞれに適切な変換を適用することができた"
 ]
