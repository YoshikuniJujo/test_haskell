import Lecture
import Whats
import Control.Monad

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第1回 ミニマルな本質"

pages :: [Page]
pages = [
	titlePage, whats1, whats2, whats3, whats4,
	attention, definition, policy,
	typeDef, baseFunDef, addSubDef, numSummary,
	posTypeDef, difDef, distDef,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

attention :: Page
attention = [\t -> do
	writeTopTitle t "これから見ていく例題について", \t -> do
	text t ""
	semititle t "例題を通してHaskellの本質を見ていきたい", \t -> do
	text t ""
	semititle t "* わずかな構文でどれだけのことができるか", \t -> do
	semititle t "* Haskellによる抽象化の本質を示す", \t -> do
	semititle t "* 実際のプログラミングのやりかたではない"
 ]

definition :: Page
definition = [\t -> do
	writeTopTitle t "問題定義"
	text t "", \t -> do
	text t "以下のような「街」における"
	text t "地点(x, y) から地点(x', y') への道のりを求める"
	forM_ [0 .. 2] $ \y -> do
		forM_ [0 .. 2] $ \x -> do
			when (y == 0) $
				graphWrite t (indexX x) (indexY y) $ show x
			when (y > 0 && x == 0) $
				graphWrite t (indexX x) (indexY y) $ show y
			drawRect t (blockX x) (blockY y) w h
			when (y == 0 && x == 2) $
				graphWrite t (indexX $ x + 1) (indexY y) $
					show $ x + 1
			when (y == 2 && x == 0) $
				graphWrite t (indexX x) (indexY $ y + 1) $
					show $ y + 1
 ]	where
 	left = 25
	top = 45
	w = 8
	h = 10
	xspace = 3
	yspace = 3
 	indexX x = left + (w + xspace) * fromIntegral x
	indexY y = top + (h + yspace) * fromIntegral y
	blockX x = left + 2.5 + (w + xspace) * fromIntegral x
	blockY y = top + (h + yspace) * fromIntegral y

policy :: Page
policy = [\t -> do
	writeTopTitle t "方針"
	text t "", \t -> do
	semititle t "* 組み込みの整数型は使わない", \t -> do
	semititle t "* 整数型とそれを処理する関数を作る", \t -> do
	semititle t "* 位置は上記の整数型を使った新しい型とする"
 ]

typeDef :: Page
typeDef = [\t -> do
	writeTopTitle t "整数型を作る"
	text t "", \t -> do
	text t "data Number = Zero | One | Two | Three"
	itext t 1 "| Four | Five | Six | Error"
	text t "", \t -> do
	text t "Zero, One, ..., Six, Errorを値とする型Numberを定義", \t -> do
	itext t 1 "* Zero, ...はリテラルとして使える"
	itext t 1 "* Zero, ...はパターンマッチで使える"
 ]

baseFunDef :: Page
baseFunDef = [\t -> do
	writeTopTitle t "基本的な関数を作る"
	text t "", \t -> do
	text t "inc Zero = One"
	text t "inc One = Two"
	text t "inc Two = Three"
	text t "inc Three = Four"
	text t "inc Four = Five"
	text t "inc Five = Six"
	text t "inc Six = Error"
	text t "inc Error = Error"
	replicateM_ 7 $ preLine t, \t -> do
	itext t 4 "dec Zero = Error"
	itext t 4 "dec One = Zero"
	itext t 4 "dec Two = One"
	itext t 4 "dec Three = Two"
	itext t 4 "dec Four = Three"
	itext t 4 "dec Five = Four"
	itext t 4 "dec Six = Five"
	itext t 4 "dec Error = Error"
	text t "", \t -> do
	text t "* それぞれの値の後(inc)と前(dec)を定義した", \t -> do
	text t "* fun [パターン] = [値]という形"
 ]

addSubDef :: Page
addSubDef = [\t -> do
	writeTopTitle t "足し算・引き算を定義"
	text t "", \t -> do
	text t "add Error _ = Error"
	text t "add _ Error = Error"
	text t "add x Zero = x"
	text t "add x y = inc (add x (dec y))", \t -> do
	arrowIText t 0 "x + 0 = x, x + y = (x + (y - 1)) + 1ということ"
	text t "", \t -> do
	text t "sub Error _ = Error"
	text t "sub _ Error = Error"
	text t "sub x Zero = x"
	text t "sub x y = dec (sub s (dec y))", \t -> do
	arrowIText t 0 "x - 0 = x, x - y = (x - (y - 1)) - 1 ということ"
 ]

numSummary :: Page
numSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* 今回の目的には0から6までの範囲の整数でよい", \t -> do
	text t "* 足し算、引き算のできる整数型を定義した"
	text t "", \t -> do
	text t "* 「型」と「関数」はセットでひとつの抽象となる", \t -> do
	text t "* incとdecに見るように関数とは入力から出力への「写像」", \t -> do
	text t "* addとsubでは「再帰的定義」という手法が使われている"
 ]

posTypeDef :: Page
posTypeDef = [\t -> do
	writeTopTitle t "位置を表すデータ構造"
	text t "", \t -> do
	text t "data Position = Position Number Number"
	text t "", \t -> do
	text t "* 前に定義したNumberを使って型Positionを定義した", \t -> do
	text t "* ふたつのPositionはそれぞれ型名と型構築子", \t -> do
	text t "* data宣言は"
	itext t 1 "data [型名] = [構築子1] [型1] [型2] ..."
	itext t 3 "| [構築子2] [型3] [型4] ..."
	itext t 3 "..."
	itext t 1 "という形"
 ]

difDef :: Page
difDef = [\t -> do
	writeTopTitle t "2つの数の差"
	text t "", \t -> do
	text t "dif x y = case sub x y of"
	itext t 1 "Error -> sub y x"
	itext t 1 "s -> s"
	text t "", \t -> do
	text t "* 関数の引数以外でパターンマッチする場合case 文を使う"
	itext t 1 "関数定義でのパターンマッチは構文糖と考えられる", \t -> do
	text t "* sub x yの値がErrorならばsub y xを返す", \t -> do
	text t "* 上以外ではsub x yの値はsにマッチしそれが返される"
 ]

distDef :: Page
distDef = [\t -> do
	writeTopTitle t "道のりを計算する関数"
	text t "", \t -> do
	text t "distance (Position x y) (Position x' y') ="
	itext t 1 "(x `dif` x') `add` (y `dif` y')"
	text t "", \t -> do
	text t "* 関数はバッククォートで囲むと演算子になる"
	itext t 1 "逆に演算子は()で囲むと関数になる", \t -> do
	text t "* x' や y' といった変数名は許される"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellの構文だけで道のりを求める関数を作成した", \t -> do
	text t "* 組み込みの型、関数、ライブラリは一切使っていない"
	text t "", \t -> do
	text t "* 抽象化という作業の本質をついた構文の持つ力", \t -> do
	text t "* それを紹介することでHaskellの魅力の一端を伝えたい", \t -> do
	text t "* Haskellの魅力はまだまだたくさんある", \t -> do
	text t "* かめばかむほど魅力的なHaskellを一緒に学んでいきたい"
 ]
