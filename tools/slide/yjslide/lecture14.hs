module Main where

import Lecture

subtitle :: String
subtitle = "第14回 モナド"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
--	monadLaws, monadLaw1, monadLaw2, monadLaw12, monadLaw3, monadLaw3',
	useMaybe, useMaybe2, useMaybe3, useMaybe4, useMaybe5, useMaybe6,
	useMaybe7, useMaybe8, useMaybe9, useMaybe10,
	state, state2, state3, state4, state5, state6,
	connect, wrap, monadBaseFun,
	monadBaseLaw, monadLaw1, monadLaw2, monadLaw3,
	whatsMonad, thatsMonad, monadClass, maybeMonad, doNotation,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* IOモナドの説明ではモナド自体の説明は省略", \t -> do
	itext t 1 "- IOモナドの本質はモナドではない", \t -> do
	itext t 1 "- モナドという方向からIOを見ると誤解が生じる", \t -> do
	itext t 1 "- IOは特殊なので他のモナドとは区別する必要がある"
	text t "", \t -> do
	text t "* モナドとは何か?", \t -> do
	itext t 1 "- 複数の型に共通する構造をくくり出したもの", \t -> do
	itext t 1 "- 一見して共通点のない構造に同じ関数が使える", \t -> do
	itext t 1 "- 具体例を見ていくのが一番わかりやすい"
 ]

{-
	itext t 1 "以下の型の関数を持つ型m", \t -> do
	itext t 1 "return :: a -> m a"
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	text t "今は理解できなくていい", \t -> do
	arrowIText t 1 "モナドの理解はじわじわと深めていけば良い"
 ]
 -}

monadLaws :: Page
monadLaws = [\t -> do
	writeTopTitle t "モナド則(前置き)"
	text t "", \t -> do
	text t "* モナドには3つの基本則がある", \t -> do
	itext t 1 "1. return x >>= f == f x", \t -> do
	itext t 1 "2. m >>= return == m", \t -> do
	itext t 1 "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)"
	text t "", \t -> do
	text t "これらについて簡単に説明していこう", \t -> do
	itext t 1 "- 今はわからなくても良い", \t -> do
	itext t 1 "- 自分でオリジナルなモナドを作るときに必要", \t -> do
	itext t 1 "- 何となくそういう決まりがある程度の理解でOK"
 ]

{-

monadLaw1 :: Page
monadLaw1 = [\t -> do
	writeTopTitle t "モナド則1"
	text t "", \t -> do
	semititle t "1. return x >>= f == f x", \t -> do
	text t "* do記法で書いてみる"
	itext t 1 "do"
	preLine t
	itext t 2 "y <- return x"
	itext t 2 "f y", \t -> do
	text t "* return xは「何もせずにxをmに包み込む」ということ", \t -> do
	text t "* 「何もせずに包み込まれたx」を取り出してfを適用する", \t -> do
	text t "* xにfを適用するということ", \t -> do
	dvArrowShort t
	text t "returnで包み込まれた値はそのまま取り出せることを保証"
 ]

monadLaw2 :: Page
monadLaw2 = [\t -> do
	writeTopTitle t "モナド則2"
	text t "", \t -> do
	semititle t "2. m >>= return == m", \t -> do
	text t "* do記法で書いてみる"
	itext t 1 "do"
	preLine t
	itext t 2 "x <- m"
	itext t 2 "return x", \t -> do
	text t "* 何かをした結果包み込まれた値を取り出す", \t -> do
	text t "* その値を「何もせずに包み込む」", \t -> do
	text t "* 取り出したものを包み込んだだけ", \t -> do
	text t "* もともとの結果と同じこと", \t -> do
	dvArrowShort t
	text t "* returnが包み込まれた値を変えないことを保証", \t -> do
	text t "* returnが包み込みかたを変えないことを保証"
 ]

monadLaw12 :: Page
monadLaw12 = [\t -> do
	writeTopTitle t "モナド則1と2"
	text t "", \t -> do
	semititle t "1. return x >>= f == f x", \t -> do
	semititle t "2. m >>= return = m", \t -> do
	text t "* returnで包み込まれた値はそのまま取り出せる", \t -> do
	text t "* returnは包み込まれた値を変えない"
	text t "* returnは包み込みかたを変えない", \t -> do
	dvArrowShort t
	text t "returnが包み込む以外に「何もしない」ことを保証している"
 ]

monadLaw3 :: Page
monadLaw3 = [\t -> do
	writeTopTitle t "モナド則3"
	text t "", \t -> do
	semititle t "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)", \t -> do
	text t "* do記法で書いてみる"
	itext t 1 "do"
	preLine t
	itext t 1.5 "y <- do"
	preLine t
	itext t 3 "x <- m"
	itext t 3 "f x"
	itext t 1.5 "g y", \t -> do
	preLine t
	preLine t
	preLine t
	itext t 5 "do"
	preLine t
	itext t 5.5 "x <- m"
	itext t 5.5 "do"
	preLine t
	itext t 6 "y <- f x"
	itext t 6 "g y", \t -> do
	text t "* mから取り出した値をfに適用したもの"
	text t "* そこから取り出した値をgに適用"
	text t "", \t -> do
	text t "* mから取り出した値"
	text t "* それをfに適用したものから取り出した値にgを適用"
 ]

monadLaw3' :: Page
monadLaw3' = [\t -> do
	writeTopTitle t "モナド則3"
	text t ""
	semititle t "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)", \t -> do
	text t "* この法則は(x + y) + z == x + (y + z)と類似", \t -> do
	text t "* つまり結合則ということ", \t -> do
	text t "* mとfを先に結合しても、fとgを先に結合しても結果は同じ"
	text t "", \t -> do
	text t "自分でモナドを作るとき", \t -> do
	dvArrowShort t
	semititle t "3つのモナド則を満たすように気をつけよう"
	text t "", \t -> do
	text t "...ここまでは前置き"
 ]

-}

maybeTitle :: String
maybeTitle = "失敗の可能性のある計算"

useMaybe :: Page
useMaybe = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* 失敗する可能性のある関数を考える", \t -> do
	text t "* ある値がリストの何番目にあるか調べる関数を考えよう", \t -> do
	itext t 1 "elemIndex1 :: Eq a => Int -> a -> [a] -> Int"
	itext t 1 "elemIndex1 n x0 (x : xs)"
	itext t 2 "| x == x0 = n"
	itext t 2 "| otherwise = elemIndex1 (n + 1) xs"
	itext t 1 "elemIndex1 _ _ [] = error \"not exist\""
	text t "", \t -> do
	arrowIText t 1 "リストに存在しない値を探した場合異常終了", \t -> do
	arrowIText t 1 "うれしくない"
 ]

useMaybe2 :: Page
useMaybe2 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* 値が存在していたならばその位置を返す", \t -> do
	text t "* 存在しなければ存在しなかったことを示す値を返す", \t -> do
	text t "* そういう型が欲しい", \t -> do
	itext t 1 "data MaybeInt = JustInt Int | NoInt", \t -> do
	text t "* もっと一般的な型を定義できる", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing"
 ]

useMaybe3 :: Page
useMaybe3 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* さっきの関数をMaybeを使って書いてみる", \t -> do
	itext t 0.5 "elemIndex2 :: Eq a => Int -> a -> [a] -> Maybe Int"
	itext t 0.5 "elemIndex2 n x0 (x : xs)"
	itext t 2 "| x == x0 = Just n"
	itext t 2 "| otheriwse = elemIndex2 (n + 1) xs"
	itext t 0.5 "elemIndex2 _ _ [] = Nothing"
	text t "", \t -> do
	arrowIText t 1 "リストに存在しない値を探した場合Nothingを返す", \t -> do
	arrowIText t 1 "いいね"
 ]

useMaybe4 :: Page
useMaybe4 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* 別の関数を考える", \t -> do
	text t "* リストのi番目を返す関数", \t -> do
	text t "* リストの長さが足りなければNothingを返す", \t -> do
	itext t 1 "maybeIndex :: [a] -> Int -> Maybe a"
	itext t 1 "maybeIndex (x : xs) 0 = Just x"
	itext t 1 "maybeIndex (x : xs) n"
	itext t 2 "| n > 0 = maybeIndex xs (n - 1)"
	itext t 1 "maybeIndex _ _ = Nothing", \t -> do
	text t "* elemIndexを使いやすくしておく"
	itext t 1 "elemIndex :: Eq a => a -> [a] -> Maybe Int"
	itext t 1 "elemIndex = elemIndex2 0"
 ]

useMaybe5 :: Page
useMaybe5 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* 以下の関数がある", \t -> do
	itext t 1 "elemIndex :: Eq a => a -> [a] -> Maybe Int"
	itext t 1 "maybeIndex :: [a] -> Int -> Maybe a", \t -> do
	text t "* あるリストでxがある位置に別のリストで何があるか", \t -> do
	itext t 1 "samePos :: Eq a => a -> [a] -> [b] -> Maybe b"
	itext t 1 "samePos x xs ys = case elemIndex x xs of"
	itext t 2 "Just i -> maybeIndex ys i"
	itext t 2 "_ -> Nothing"
 ]

useMaybe6 :: Page
useMaybe6 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* ある数で100をわった時のあまりを求める関数", \t -> do
	itext t 1 "mod100 :: Int -> Maybe Int"
	itext t 1 "mod100 0 = Nothing"
	itext t 1 "mod100 n = Just $ 100 `mod` n", \t -> do
	text t "* あるリストでxがある位置に別のリストで何があるか", \t -> do
	text t "* それを求めてさらにそれで100をわったあまりを求める関数", \t -> do
	itext t 0.5 "samePosMod :: Eq a => a -> [a] -> [Int] -> Maybe Int"
	itext t 0.5 "samePosMod x xs ys = case elemIndex x xs of"
	itext t 2 "Just i -> case maybeIndex ys i of"
	itext t 3 "Just n -> mod100 n"
	itext t 3 "_ -> Nothing"
	itext t 2 "_ -> Nothing"
 ]

useMaybe7 :: Page
useMaybe7 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* 何度も出てくる構造がある", \t -> do
	itext t 1 "case m of"
	itext t 2 "Just x -> case f x of"
	itext t 3 "Just y -> case g y of"
	itext t 4 "Just z -> ..."
	itext t 4 "_ -> Nothing"
	itext t 3 "_ -> Nothing"
	itext t 2 "_ -> Nothing", \t -> do
	text t "* 計算の途中に失敗があれば全体も失敗するような構造", \t -> do
	text t "* m, f, gの型はMaybe a, a -> Maybe b, b -> Maybe c"
 ]

useMaybe8 :: Page
useMaybe8 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "* 以下の構造を作る高階関数を作る", \t -> do
	itext t 1 "case m of"
	itext t 2 "Just x -> f x"
	itext t 2 "_ -> Nothing", \t -> do
	text t "* とりあえずpipeという名前にしておく", \t -> do
	itext t 1 "pipe :: Maybe a -> (a -> Maybe b) -> Maybe b"
	itext t 1 "pipe (Just x) f = f x"
	itext t 1 "pipe _ _ = Nothing", \t -> do
	text t "* さっきの構造は以下のように表せる", \t -> do
	itext t 1 "m `pipe` f `pipe` g ..."
 ]

useMaybe9 :: Page
useMaybe9 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	text t "elemIndex :: Eq a => a -> [a] -> Maybe Int"
	text t "maybeIndex :: [a] -> Int -> Maybe a"
	text t "mod100 :: Int -> Maybe Int"
	text t "", \t -> do
	text t "samePosMod100 :: Eq a => a -> [a] -> [Int] -> Maybe Int", \t -> do
	text t "samePosMod100 x xs ys ="
	itext t 1 "elemIndex x xs `pipe` maybeIndex ys `pipe` mod100"
	text t "", \t -> do
	text t "* インデックスを求め整数を取り出し100をわったあまりを", \t -> do
	text t "* 途中でNothingとなれば結果もNothingとなる"
 ]

useMaybe10 :: Page
useMaybe10 = [\t -> do
	writeTopTitle t maybeTitle
	text t "", \t -> do
	semititle t "pipe :: Maybe a -> (a -> Maybe b) -> Maybe b"
	text t "", \t -> do
	text t "* 以下の構造を作る高階関数", \t -> do
	itext t 1 "case m of"
	itext t 2 "Just x -> f x"
	itext t 2 "_ -> Nothing", \t -> do
	text t "* 意味としては", \t -> do
	itext t 1 "- 計算に失敗しなければ次の関数に値をわたす", \t -> do
	itext t 1 "- 失敗したらそれ以降はNothingをわたしていく"
 ]

stateTitle :: String
stateTitle = "状態を持った計算"

state :: Page
state = [\t -> do
	writeTopTitle t stateTitle
	text t "", \t -> do
	text t "* 状態を変化させながらプログラムするというパラダイム", \t -> do
	text t "* それを関数型言語のなかでエミュレートする", \t -> do
	text t "* 状態を取って値と状態を返す関数を考える", \t -> do
	itext t 1 "s -> (a, s)", \t -> do
	text t "* メモリ機能付きの電卓を考えてみる", \t -> do
	text t "* 状態モナドについては後でまた扱う", \t -> do
	itext t 1 "- ここでは細かいところは理解しなくても良い"
 ]

state2 :: Page
state2 = [\t -> do
	writeTopTitle t stateTitle, \t -> do
	text t "* 状態を使う関数の型に名前をつける", \t -> do
	itext t 1 "type Calc a = Int -> (a, Int)", \t -> do
	text t "* メモリを扱う関数を作る", \t -> do
	itext t 1 "memoryClear :: Calc ()"
	itext t 1 "memoryClear = \\_ -> ((), 0)"
	itext t 1 "", \t -> do
	itext t 1 "memoryPlus, memoryMinus :: Int -> Calc ()"
	itext t 1 "memoryPlus x = \\m -> ((), m + x)", \t -> do
	itext t 1 "memoryMinus x = \\m -> ((), m - x)"
	itext t 1 "", \t -> do
	itext t 1 "memoryRecall :: Calc Int"
	itext t 1 "memoryRecall = \\m -> (m, m)"
 ]

state3 :: Page
state3 = [\t -> do
	writeTopTitle t stateTitle
	text t "", \t -> do
	text t "* ただの数をCalcにする関数", \t -> do
	itext t 1 "liftNum :: Int -> Calc Int"
	itext t 1 "liftNum x = \\m -> (x, m)", \t -> do
	text t "* 計算をつなげる関数", \t -> do
	itext t 1 "(>>>=) :: Calc a -> (a -> Calc b) -> Calc b"
	itext t 1 "c >>>= f = \\m -> let (x, m') = c m in f x m'", \t -> do
	itext t 1 "- 計算cの結果をfに適用するということ", \t -> do
	itext t 1 "- cにメモリの値mを与える", \t -> do
	itext t 1 "- 結果xと新しいメモリの状態m'が得られる", \t -> do
	itext t 1 "- 結果xと新しいメモリの状態m'をfに与える"
 ]

state4 :: Page
state4 = [\t -> do
	writeTopTitle t stateTitle
	text t "", \t -> do
	text t "* 計算を実行する関数", \t -> do
	itext t 1 "runCalc :: StateInt a -> a"
	itext t 1 "runCalc c = fst $ c 0", \t -> do
	itext t 1 "- メモリの初期値0を与えfstで結果を取り出す"
 ]

state5 :: Page
state5 = [\t -> do
	writeTopTitle t stateTitle
	text t "", \t -> do
	text t "* (3 + 2) * (11 - 3)を計算してみる"
	itext t 1 "calc :: Calc Int"
	itext t 1 "calc ="
	preLine t
	itext t 2.5 "memoryPlus 3 >>>= \\_ ->"
	itext t 2.5 "memoryPlus 2 >>>= \\_ ->"
	itext t 2.5 "memoryRecall >>>= \\x ->"
	itext t 2.5 "memoryClear >>>= \\_ ->"
	itext t 2.5 "memoryPlus 11 >>>= \\_ ->"
	itext t 2.5 "memoryMinus 3 >>>= \\_ ->"
	itext t 2.5 "memoryRecall >>>= \\y ->"
	itext t 2.5 "liftNum (x * y)"
 ]

state6 :: Page
state6 = [\t -> do
	writeTopTitle t stateTitle
	text t "", \t -> do
	text t "> runCalc calc"
	text t "40", \t -> do
	text t "状態を持った計算をエミュレートできた", \t -> do
	semititle t "(>>>=) :: Calc a -> (a -> Calc b) -> Calc b", \t -> do
	text t "* 見えないところで状態mをわたしている", \t -> do
	text t "* 状態を扱う関数は別に定義してある", \t -> do
	itext t 1 "- 状態を変化させる関数", \t -> do
	itext t 1 "- 状態を結果として見えるようにする関数", \t -> do
	semititle t "liftNumber :: Int -> Calc Int", \t -> do
	text t "* 普通の数を計算のなかに入れるための関数"
 ]

connect :: Page
connect = [\t -> do
	writeTopTitle t "「つなぐ」関数", \t -> do
	text t "* 失敗の可能性のある計算", \t -> do
	semititle t "pipe :: Maybe a -> (a -> Maybe b) -> Maybe b"
	text t "", \t -> do
	text t "* 状態を持つ計算", \t -> do
	semititle t "(>>>=) :: Calc a -> (a -> Calc b) -> Calc b"
	text t "", \t -> do
	text t "* 入出力を扱う計算", \t -> do
	semititle t "(>>=) :: IO a -> (a -> IO b) -> IO b"
	text t "", \t -> do
	text t "* 型のなかに共通の構造がある", \t -> do
	semititle t "(>>=) :: m a -> (a -> m b) -> m b"
 ]

wrap :: Page
wrap = [\t -> do
	writeTopTitle t "「つつむ」関数", \t -> do
	text t "* 失敗の可能性のある計算", \t -> do
	semititle t "Just :: a -> Maybe a"
	text t "", \t -> do
	text t "* 状態を持つ計算", \t -> do
	semititle t "liftNumber :: Int -> Calc Int"
	text t "", \t -> do
	text t "* 入出力を扱う計算", \t -> do
	semititle t "return :: a -> IO a"
	text t "", \t -> do
	text t "* 型のなかに共通の構造がある", \t -> do
	semititle t "return :: a -> m a"
 ]

monadBaseFun :: Page
monadBaseFun = [\t -> do
	writeTopTitle t "モナドの持つ2つの関数"
	text t "", \t -> do
	semititle t "return :: a -> m a", \t -> do
	semititle t "(>>=) :: m a -> (a -> m b) -> m b"
	text t "", \t -> do
	text t "* returnは裸の値を包み込む関数", \t -> do
	text t "* m >>= fを考えてみる", \t -> do
	itext t 1 "- mは包まれた値", \t -> do
	itext t 1 "- fは値を加工しながら包みこむ関数", \t -> do
	itext t 1 "- (>>=)は包装を解いてfに渡す関数", \t -> do
	text t "* 裸の値を包みこむ関数に、包装された値を与えられる", \t -> do
	text t "* 実例を多く見ていくことが理解するこつ"
 ]

monadBaseLaw :: Page
monadBaseLaw = [\t -> do
	writeTopTitle t "モナド則"
	text t "", \t -> do
	text t "* モナドがモナドであるために3つの法則がある", \t -> do
	text t "* モナド則は以下のことを保証している", \t -> do
	itext t 0.5 "- returnが包み込む以外何もしないということ...(1)", \t -> do
	itext t 0.5 "- 左結合でも右結合でも結果が同じであること...(2)", \t -> do
	text t "* モナド則", \t -> do
	itext t 0.5 "1. return x >>= f <-(同じ)-> f x", \t -> do
	itext t 0.5 "2. m >>= return <-(同じ)-> m", \t -> do
	itext t 0.5 "3. (m >>= f) >>= g <-(同じ)-> m >>= (\\x -> f x >>= g)", \t -> do
	text t "* モナド則1と2は(1)を保証している", \t -> do
	text t "* モナド則3が(2)を保証している", \t -> do
	text t "* それぞれ見ていこう"
 ]

monadLaw1 :: Page
monadLaw1 = [\t -> do
	writeTopTitle t "モナド則1"
	text t "", \t -> do
	semititle t "1. return x >>= f <-(同じ)-> f x"
	text t "", \t -> do
	text t "* 以下が等しいということ", \t -> do
	itext t 1 "- 包み込んだものの包装を解いてfに与えてる", \t -> do
	itext t 1 "- そのままの値をfに与える", \t -> do
	text t "* returnが値を変化させないことを保証している", \t -> do
	itext t 1 "- 正確には値を変化させないように「見える」", \t -> do
	itext t 1 "- 包み込みと包装を解くこととが逆関数であれば良い", \t -> do
	text t "* returnで包み込んだあと包装を解けば同じものになる"
 ]

monadLaw2 :: Page
monadLaw2 = [\t -> do
	writeTopTitle t "モナド則2"
	text t "", \t -> do
	semititle t "2. m >>= return <-(同じ)-> m"
	text t "", \t -> do
	text t "* 以下が等しいということ", \t -> do
	itext t 1 "- 包装されたものの包装を解きそれを包み込む", \t -> do
	itext t 1 "- もともとの包装されたもの", \t -> do
	text t "* returnは包みかたを変化させないことを保証している", \t -> do
	itext t 1 "- 正確には包みかたを変化させないように「見える」", \t -> do
	text t "* 包装を解いたものをreturnで包み込めば同じものになる"
 ]

monadLaw3 :: Page
monadLaw3 = [\t -> do
	writeTopTitle t "モナド則3"
	text t "", \t -> do
	semititle t "3. (m >>= f) >>= g"
	isemititle t 1 "<-(同じ)-> m >>= (\\x -> f x >>= g)", \t -> do
	text t "* これは形を変えたほうがわかりやすい", \t -> do
	text t "* 以下の関数を考える", \t -> do
	itext t 1 "(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)"
	itext t 1 "f >=> g = \\x -> f x >>= g", \t -> do
	text t "* すると以下のようになる"
	text t "", \t -> do
	semititle t "3. (f >=> g) >=> h <-(同じ)-> f >=> (g >=> h) ", \t -> do
	text t "* つまり結合則である"
 ]

whatsMonad :: Page
whatsMonad = [\t -> do
	writeTopTitle t "モナドとは?"
	text t "", \t -> do
	text t "* 以下の型の関数を持つ", \t -> do
	itext t 1 "return :: a -> m a", \t -> do
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	text t "* それらの関数が以下の法則を満たす", \t -> do
	itext t 1 "1. return x >>= f == f x", \t -> do
	itext t 1 "2. m >>= return == m", \t -> do
	itext t 1 "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)"
	dvArrowShort t
	itext t 2 "モナド"
 ]

thatsMonad :: Page
thatsMonad = [\t -> do
	writeTopTitle t "モナドとは?"
	text t "", \t -> do
	text t "* 特定の構造を持った関数によって扱える容器", \t -> do
	text t "* それ以外の条件はない", \t -> do
	arrowIText t 1 "まったく違うものが同じモナドとして共通に扱える"
	text t "", \t -> do
	text t "* 中身は違うけど共通した構造を持つもの", \t -> do
	arrowIText t 1 "Haskellでは型クラスでまとめて扱える", \t -> do
	dvArrowShort t
	text t "Monadクラスが用意されている"
 ]

monadClass :: Page
monadClass = [\t -> do
	writeTopTitle t "Monadクラス"
	text t "", \t -> do
	text t "* 定義は以下のような感じ", \t -> do
	itext t 1 "class Monad m where"
	itext t 2 "(>>=) :: m a -> (a m b) -> m b"
	itext t 2 "return :: a -> m a"
	itext t 2 "fail :: String -> m a", \t -> do
	text t "* 本質的にはfailは関係ない", \t -> do
	itext t 1 "- do記法でパターンマッチが失敗したときのため", \t -> do
	itext t 1 "- 失敗をうまく扱えるタイプのモナドでは有用"
 ]

maybeMonad :: Page
maybeMonad = [\t -> do
	writeTopTitle t "Maybe monad"
	text t "", \t -> do
	text t "* MaybeはMonadクラスのインスタンスである", \t -> do
	text t "* 定義は以下のようになっている", \t -> do
	itext t 1 "instance Monad Maybe where"
	itext t 2 "Just x >>= k = k x"
	itext t 2 "Nothing >>= _ = Nothing"
	itext t 2 "return = Just"
	itext t 2 "fail _ = Nothing"
 ]

doNotation :: Page
doNotation = [\t -> do
	writeTopTitle t "do記法"
	text t "", \t -> do
	text t "* IOモナドのところでdo記法を学んだ", \t -> do
	itext t 1 "- Monadクラスのインスタンスであれば何にでも", \t -> do
	text t "* Maybeモナドで見てみよう", \t -> do
	itext t 1 "samePosMod100 x xs ys = do"
	itext t 2 "i <- elemIndex x xs"
	itext t 2 "n <- maybeIndex ys i"
	itext t 2 "mod100 n"
	text t "", \t -> do
	arrowIText t 1 "読みやすくなった"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* モナドは以下の関数を持つ", \t -> do
	itext t 1 "return :: a -> m a", \t -> do
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	text t "* 上記関数が以下の法則を満たせばmはモナドである", \t -> do
	itext t 1 "return x >>= f == f x", \t -> do
	itext t 1 "m >>= return == m", \t -> do
	itext t 1 "(m >>= f) >>= g == m >>= (\\x -> f x >>= g)", \t -> do
	text t "* Haskellには型クラスMonadが用意されている", \t -> do
	text t "* Monadクラスのインスタンスにする", \t -> do
	itext t 1 "- 多くのポリモルフィックな関数が使える", \t -> do
	itext t 1 "- do記法が使える"
 ]
