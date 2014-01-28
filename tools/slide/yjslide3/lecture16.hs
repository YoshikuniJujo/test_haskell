import Data.Char

import Lecture

subtitle :: String
subtitle = "第16回 モナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutMaybe, aboutMaybe2, aboutMaybe3, aboutMaybe4, aboutMaybe5,
	aboutMaybe6, aboutMaybe7, aboutMaybe8, aboutMaybe9, aboutMaybe10,
	aboutMaybe11, aboutMaybe12, aboutMaybe13, aboutMaybe14,
	aboutMaybeSummary,
	aboutState, aboutState2, aboutState3, aboutState4, aboutState5,
	aboutState6, aboutState7, aboutState8, aboutState9, aboutState10,
	aboutState11, aboutState12, aboutState13, aboutState14, aboutState15,
	aboutState16
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* (a -> m b)の形をとるいろいろな関数が存在する", \t -> do
	text t "* この形の関数の多くは(a -> m b)と(b -> m c)をつないで", \t -> do
	itext t 1 "(a -> m c)を導出できると便利なことが多い", \t -> do
	text t "* 例としてMaybeについて考えてみる", \t -> do
	text t "* 小文字のみ文字コードにする関数と偶数のみを2で割る関数", \t -> do
	itext t 1 "lowerToCode :: Char -> Maybe Int", \t -> do
	itext t 1 "evenDiv2 :: Int -> Maybe Int", \t -> do
	text t "* これらをつないで", \t -> do
	itext t 1 "- 小文字の文字コードで偶数のものの半分の値", \t -> do
	itext t 1 "lowerToCodeDiv2 :: Char -> Maybe Int", \t -> do
	text t "* のような感じ"
 ]

aboutMaybe :: Page
aboutMaybe = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* さっきの例を実際に作ってみる", \t -> do
	text t "* lectures/lecture16/maybe.hsを作成し編集しよう", \t -> do
	text t "* Data.CharのisLower, ordを使うので", \t -> do
	itext t 1 "import Data.Char (isLower, ord)", \t -> do
	text t "* 小文字のみ文字コードにする関数", \t -> do
	itext t 1 "lowerToCode :: Char -> Maybe Int", \t -> do
	itext t 1 "lowerToCode c", \t -> do
	itext t 2 "| isLower c = Just $ ord c", \t -> do
	itext t 2 "| otherwise = Nothing", \t -> do
	text t "* maybe.hsに書き込もう"
 ]

lowerToCode :: Char -> Maybe Int
lowerToCode c
	| isLower c = Just $ ord c
	| otherwise = Nothing

aboutMaybe2 :: Page
aboutMaybe2 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* 偶数のみを2で割る関数", \t -> do
	itext t 1 "evenDiv2 :: Int -> Maybe Int", \t -> do
	itext t 1 "evenDiv2 n", \t -> do
	itext t 2 "| even n = Just $ n `div` 2", \t -> do
	itext t 2 "| otherwise = Nothing", \t -> do
	text t "* 以上をmaybe.hsに書き込もう", \t -> do
	text t "* この2つの関数をつなげよう", \t -> do
	itext t 1 "lowerToCode :: Char -> Maybe Int", \t -> do
	itext t 1 "evenDiv2 :: Int -> Maybe Int", \t -> do
	text t "* lowerToCodeが返すIntをevenDiv2の引数にしたい", \t -> do
	itext t 1 "lowerToCodeDiv2 :: Char -> Maybe Int"
 ]

evenDiv2 :: Int -> Maybe Int
evenDiv2 n
	| even n = Just $ n `div` 2
	| otherwise = Nothing

aboutMaybe3 :: Page
aboutMaybe3 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* lowerToCodeDiv2を定義しよう", \t -> do
	itext t 1 "lowerToCodeDiv2 :: Char -> Maybe Int", \t -> do
	itext t 1 "lowerToCodeDiv2 c = case lowerToCode c of", \t -> do
	itext t 2 "Just n -> evenDiv2 n", \t -> do
	itext t 2 "Nothing -> Nothing", \t -> do
	text t "* maybe.hsに書き込もう", \t -> do
	itext t 1 "% ghci maybe.hs", \t -> do
	itext t 1 "*Main> lowerToCodeDiv2 'n'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv2 'n', \t -> do
	itext t 1 "*Main> lowerToCodeDiv2 'm'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv2 'm'
 ]

lowerToCodeDiv2 :: Char -> Maybe Int
lowerToCodeDiv2 c = case lowerToCode c of
	Just n -> evenDiv2 n
	Nothing -> Nothing

aboutMaybe4 :: Page
aboutMaybe4 = [\t -> do
	writeTopTitle t "Maybe", \t -> do
	text t "* 4で割り切れるもののみを4で割るようにしてみる", \t -> do
	text t "* 以下の3つをつなぐ", \t -> do
	itext t 1 "lowerToCode :: Char -> Maybe Int", \t -> do
	itext t 1 "evenDiv2 :: Int -> Maybe Int", \t -> do
	itext t 1 "evenDiv2 :: Int -> Maybe Int", \t -> do
	text t "* 定義は以下のようになる", \t -> do
	itext t 1 "lowerToCodeDiv4 :: Char -> Maybe Int", \t -> do
	itext t 1 "lowerToCodeDiv4 c = case lowerToCode c of", \t -> do
	itext t 2 "Just n -> case evenDiv2 n of", \t -> do
	itext t 3 "Just n' -> evenDiv2 n'", \t -> do
	itext t 3 "Nothing -> Nothing", \t -> do
	itext t 2 "Nothing -> Nothing"
 ]

aboutMaybe5 :: Page
aboutMaybe5 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* どのように「つないだ」かを考えよう", \t -> do
	itext t 1 "f :: a -> Maybe b", \t -> do
	itext t 1 "g :: b -> Maybe c", \t -> do
	text t "* 関数fの結果がJust xならばxの値にgを適用する", \t -> do
	itext t 1 "- その結果はJust yまたはNothingとなる", \t -> do
	text t "* 関数fの結果がNothingなら結果もNothing"
 ]

pipeM :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f `pipeM` g = \v -> case f v of
	Just x -> g x
	Nothing -> Nothing

lowerToCodeDiv4 :: Char -> Maybe Int
lowerToCodeDiv4 = lowerToCode `pipeM` evenDiv2 `pipeM` evenDiv2

aboutMaybe6 :: Page
aboutMaybe6 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* Maybe型を返す関数をつなぐpipeMを書く", \t -> do
	itext t 0 "pipeM ::"
	itext t 1 "(a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)", \t -> do
	itext t 0 "f `pipeM` g = \\v -> case f v of", \t -> do
	itext t 1 "Just x -> g x", \t -> do
	itext t 1 "Nothing -> Nothing", \t -> do
	text t "* maybe.hsに書き込もう"
 ]

aboutMaybe7 :: Page
aboutMaybe7 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* pipeMを使うとlowerToCodeDiv4は以下のように書ける", \t -> do
	itext t 0 "lowerToCodeDiv4 :: Char -> Maybe Int", \t -> do
	itext t 0 "lowerToCodeDiv4 ="
	itext t 1 "lowerToCode `pipeM` evenDiv2 `pipeM` evenDiv2", \t -> do
	text t "* maybe.hsに書き込み、:reload", \t -> do
	itext t 1 "*Main> lowerToDiv4 'n'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv4 'n', \t -> do
	itext t 1 "*Main> lowerToDiv4 'p'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv4 'p'
 ]

aboutMaybe8 :: Page
aboutMaybe8 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* 2で割ったうえに3をかけることを考える", \t -> do
	text t "* かけ算はとくに割り切れないとかがないので", \t -> do
	itext t 1 "mul3 :: Int -> Int", \t -> do
	itext t 1 "mul3 = (* 3)", \t -> do
	text t "* しかし、pipeMでつなぐには以下の形のほうが良い", \t -> do
	itext t 1 "Int -> Maybe Int", \t -> do
	text t "* 以下の変換を行う関数を作ろう", \t -> do
	itext t 1 "arrM :: (a -> b) -> (a -> Maybe b)", \t -> do
	itext t 1 "arrM f = \\x -> Just $ f x", \t -> do
	text t "* mul3とarrMをmaybe.hsに書き込もう"
 ]

mul3 :: Int -> Int
mul3 = (* 3)

arrM :: (a -> b) -> (a -> Maybe b)
arrM f = \x -> Just $ f x

aboutMaybe9 :: Page
aboutMaybe9 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* 小文字のコードを2で割ったうえに3をかける関数は", \t -> do
	itext t 0 "lowerToCodeDiv2Mul3 :: Char -> Maybe Int", \t -> do
	itext t 0 "lowerToCodeDiv2Mul3 =", \t -> do
	itext t 1 "lowerToCode `pipeM` evenDiv2 `pipeM` arrM mul3", \t -> do
	text t "* maybe.hsに書き込み、:reloadする", \t -> do
	itext t 1 "*Main> lowerToCodeDiv2Mul3 'n'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv2Mul3 'n'
 ]

lowerToCodeDiv2Mul3 :: Char -> Maybe Int
lowerToCodeDiv2Mul3 = lowerToCode `pipeM` evenDiv2 `pipeM` arrM mul3

aboutMaybe10 :: Page
aboutMaybe10 = [\t -> do
	writeTopTitle t "Maybe"
	itext t (-1) "", \t -> do
	itext t (-1) "* (a -> Maybe b)型の関数をつなぐために用意した関数", \t -> do
	itext t (-1) "pipeM :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)", \t -> do
	itext t (-1) "arrM :: (a -> b) -> (a -> Maybe b)", \t -> do
	itext t (-1) "* 全回の講義を思い出してみよう", \t -> do
	itext t (-1) "* 引数の型と結果の型の両方に(a ->)があるので", \t -> do
	itext t 0 "- それらを消すことができる", \t -> do
	itext t 0 "bindM :: Maybe b -> (b -> Maybe c) -> Maybe c", \t -> do
	itext t 0 "retM :: b -> Maybe b", \t -> do
	itext t (-1) "* こちらのセットを定義してみよう"
 ]

bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
bindM (Just x) f = f x
bindM Nothing _ = Nothing

retM :: a -> Maybe a
retM = Just

aboutMaybe11 :: Page
aboutMaybe11 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* それぞれの関数を定義する", \t -> do
	itext t 1 "bindM :: Maybe a -> (a -> Maybe b) -> Maybe b", \t -> do
	itext t 1 "bindM (Just x) f = f x", \t -> do
	itext t 1 "bindM Nothing _ = Nothing", \t -> do
	itext t 1 "retM :: a -> Maybe a", \t -> do
	itext t 1 "retM = Just", \t -> do
	text t "* maybe.hsに書き込もう"
 ]

aboutMaybe12 :: Page
aboutMaybe12 = [\t -> do
	writeTopTitle t "Maybe", \t -> do
	text t "* このセットを使って、lowerToCodeDiv2Mul3'を定義してみる", \t -> do
	itext t 0 "lowerToCodeDiv2Mul3' :: Char -> Maybe Int", \t -> do
	itext t 0 "lowerToCodeDiv2Mul3' c ="
	itext t 0.5 "lowerToCode c `bindM` evenDiv2 `bindM` (retM . mul3)", \t -> do
	text t "* この関数を以下のように書くこともできる", \t -> do
	itext t 0 "lowerToCodeDiv2Mul3' c =", \t -> do
	itext t 1 "lowerToCode c `bindM` \\n ->", \t -> do
	itext t 1 "evenDiv2 n `bindM` \\n' ->", \t -> do
	itext t 1 "retM $ mul3 n'", \t -> do
	text t "* この形だと、lowerToCode cの結果をnに束縛し", \t -> do
	itext t 1 "- evenDiv2 nの結果をn'に束縛し", \t -> do
	itext t 1 "- mul3 nの値を返す、と読める", \t -> do
	text t "* どちらかをmaybe.hsに書き込もう"
 ]

lowerToCodeDiv2Mul3' :: Char -> Maybe Int
lowerToCodeDiv2Mul3' c =
	lowerToCode c `bindM` \n ->
	evenDiv2 n `bindM` \n' ->
	retM $ mul3 n'

aboutMaybe13 :: Page
aboutMaybe13 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> lowerToCodeDiv2Mul3' 'p'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv2Mul3' 'p'
 ]

aboutMaybe14 :: Page
aboutMaybe14 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* 2つめの形を再掲する", \t -> do
	itext t 1 "lowerToCodeDiv2Mul3' c =", \t -> do
	itext t 2 "lowerToCode c `bindM` \\n ->", \t -> do
	itext t 2 "evenDiv2 n `bindM` \\n' ->", \t -> do
	itext t 2 "retM $ mul3 n'", \t -> do
	text t "* 適切に括弧をつけると以下のようになる", \t -> do
	itext t 0 "lowerToCode c `bindM` (\\n ->", \t -> do
	itext t 1 "evenDiv2 n `bindM` (\\n' ->", \t -> do
	itext t 2 "retM $ mul3 n'))"
 ]

aboutMaybeSummary :: Page
aboutMaybeSummary = [\t -> do
	writeTopTitle t "Maybe(まとめ)"
	text t "", \t -> do
	text t "* 失敗するかもしれない計算", \t -> do
	itext t 1 "a -> Maybe b", \t -> do
	text t "* そういった計算を「つなぐ」関数をつくると", \t -> do
	itext t 1 "(a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)", \t -> do
	text t "* きれいに抽象化できる", \t -> do
	text t "* 普通の計算も同じ形に直すことで「つなぐ」", \t -> do
	itext t 1 "(a -> b) -> (a -> Maybe b)", \t -> do
	text t "* これらの関数はより単純な形に直すことができる", \t -> do
	itext t 1 "bindM :: Maybe a -> (a -> Maybe b) -> Maybe b", \t -> do
	itext t 1 "retM :: a -> Maybe a"
 ]

aboutState :: Page
aboutState = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* メモリ機能付きの電卓について考えてみよう", \t -> do
	text t "* (3 * 4 + 2 * 5) * 7の計算をしてみる", \t -> do
	text t "* 以下の順にボタンを押す", \t -> do
	itext t 1 "3 * 4 M+ C 2 * 5 C M+ MR * 7", \t -> do
	itext t 1 "- 3 * 4を計算しメモリに足す", \t -> do
	itext t 1 "- 表示を0にもどし", \t -> do
	itext t 1 "- 2 * 5を計算しメモリに足す", \t -> do
	itext t 1 "- 表示を0にもどし", \t -> do
	itext t 1 "- メモリを呼び出す", \t -> do
	itext t 1 "- 7をかける", \t -> do
	text t "* メモリ内の記憶を状態として持っていると考えられる"
 ]

aboutState2 :: Page
aboutState2 = [\t -> do
	writeTopTitle t "State", \t -> do
	text t "* それぞれの操作は", \t -> do
	itext t 1 "- 画面の表示とメモリの状態を引数として取り", \t -> do
	itext t 1 "- 画面の表示とメモリの状態を返値として返す", \t -> do
	text t "* 実際の電卓とは異なるが", \t -> do
	itext t 1 "- M+は画面をクリアするものとする(つまりCも行う)", \t -> do
	itext t 1 "- MRを押す前に画面がクリアされている必要がある", \t -> do
	itext t 1 "- つまり、MRは画面の状態を受け取らない", \t -> do
	text t "* よって、それぞれの型は以下のようになる", \t -> do
	itext t 1 "mplus :: Int -> Int -> ((), Int)", \t -> do
	itext t 1 "mrecall :: () -> Int -> (Int, Int)", \t -> do
	text t "* それぞれ返値と引数の画面の表示の部分が()としてある", \t -> do
	text t "* 表示がないということを()で表現している"
 ]

aboutState3 :: Page
aboutState3 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 次に画面を変化させる関数を作る", \t -> do
	text t "* (Int -> Int)関数を変換することによって作ることにする", \t -> do
	text t "* メモリは変化させない、つまり", \t -> do
	itext t 1 "- メモリの値を引数として取り、そのまま返り値とする", \t -> do
	itext t 1 "arrC :: (Int -> Int) -> Int -> Int -> (Int, Int)", \t -> do
	text t "* arrCは単なる数字キーにも対応させたい", \t -> do
	text t "* 数字キーは引数を取らないでIntを返す関数なので", \t -> do
	itext t 1 "() -> Int", \t -> do
	itext t 1 "arrC :: (() -> Int) -> () -> Int -> (Int, Int)", \t -> do
	text t "* よってarrCはより一般的に以下のようにする", \t -> do
	itext t 1 "arrC :: (a -> Int) -> a -> Int -> (Int, Int)"
 ]

aboutState4 :: Page
aboutState4 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* arrCを画面のクリアにも使えるようにしたい", \t -> do
	text t "* 画面をクリアする関数は(Int -> ())", \t -> do
	itext t 1 "arrC :: (Int -> ()) -> Int -> Int -> ((), Int)", \t -> do
	text t "* さっきまでのarrCの型が以下のようになっていたので", \t -> do
	itext t 1 "arrC :: (a -> Int) -> a -> Int -> (Int, Int)", \t -> do
	text t "* 十分に一般的なarrCの型は以下のようになる", \t -> do
	itext t 1 "arrC :: (a -> b) -> a -> Int -> (b, Int)"
 ]

aboutState5 :: Page
aboutState5 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 必要な関数は以下のようになる", \t -> do
	itext t 1 "mplus :: Int -> Int -> ((), Int)", \t -> do
	itext t 1 "mrecall :: () -> Int -> (Int, Int)", \t -> do
	itext t 1 "arrC :: (a -> b) -> a -> Int -> (b, Int)", \t -> do
	text t "* mplusを定義してみよう", \t -> do
	itext t 1 "- 画面の表示とメモリの内容を足してメモリに保存する", \t -> do
	itext t 1 "- 画面の表示はクリアする", \t -> do
	itext t 1 "mplus x m = ((), x + m)"
 ]

mplus :: Int -> Int -> ((), Int)
mplus x m = ((), x + m)

mrecall :: () -> Int -> (Int, Int)
mrecall _ m = (m, m)

aboutState6 :: Page
aboutState6 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* calc.hsを作成し以下を書き込もう", \t -> do
	itext t 1 "mplus :: Int -> Int -> ((), Int)", \t -> do
	itext t 1 "mplus x m = ((), x + m)", \t -> do
	text t "* mrecallを定義する", \t -> do
	itext t 1 "- メモリの内容を画面に呼び出す", \t -> do
	itext t 1 "- 呼び出した直後はメモリと画面は同じ値になる", \t -> do
	itext t 1 "mrecall :: () -> Int -> (Int, Int)", \t -> do
	itext t 1 "mrecall _ m = (m, m)", \t -> do
	text t "* これもcalc.hsに書き込もう"
 ]

aboutState7 :: Page
aboutState7 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :load calc.hs", \t -> do
	itext t 1 "*Main> mplus 3 0", \t -> do
	itext t 1 $ show $ mplus 3 0, \t -> do
	itext t 1 "*Main> mplus 4 8", \t -> do
	itext t 1 $ show $ mplus 4 8, \t -> do
	text t "* 画面の表示が3でメモリが0のときmplusをすると", \t -> do
	itext t 1 "- 画面はクリア(ユニット値, 0ではなく())され", \t -> do
	itext t 1 "- メモリは3となる", \t -> do
	text t "* 画面の表示が4でメモリが8のときmplusをすると", \t -> do
	itext t 1 "- 画面はクリアされ、メモリは12となる"
 ]

aboutState8 :: Page
aboutState8 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> mrecall () 4", \t -> do
	itext t 1 $ show $ mrecall () 4, \t -> do
	itext t 1 "*Main> mrecall () 19", \t -> do
	itext t 1 $ show $ mrecall () 19, \t -> do
	text t "* mrecallをする前には画面がクリアされている必要がある", \t -> do
	itext t 1 "- つまり画面はユニット値になっていなければならない", \t -> do
	text t "* mrecallをするとメモリの内容が画面に呼び出される", \t -> do
	itext t 1 "- mrecallの直後はメモリの内容と画面は同じ値になる"
 ]

aboutState9 :: Page
aboutState9 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 次に必要なのは関数によって電卓の表示を変化させる関数", \t -> do
	itext t 1 "arrC :: (a -> b) -> a -> Int -> (b, Int)", \t -> do
	text t "* メモリの値は変化させない、つまり", \t -> do
	itext t 1 "- 引数として取りそのまま返す", \t -> do
	text t "* 画面の値は与えられた関数で変化させる", \t -> do
	itext t 1 "arrC f x m = (f x, m)", \t -> do
	text t "* これらをcalc.hsに書き込もう"
 ]

arrC :: (a -> b) -> a -> Int -> (b, Int)
arrC f x m = (f x, m)

aboutState10 :: Page
aboutState10 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	text t "* まずは単なる数字を入れてみる", \t -> do
	text t "* 単なる数字を入れるには", \t -> do
	itext t 1 "- 画面の値を無視して数を返す関数を使えば良いので", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> arrC (const 8) () 4", \t -> do
	itext t 1 $ show $ arrC (const (8 :: Int)) () 4, \t -> do
	itext t 1 "*Main> arrC (const 11) () 32", \t -> do
	itext t 1 $ show $ arrC (const (11 :: Int)) () 32, \t -> do
	text t "* メモリの値は変化せずに表示が与えられた数となる", \t -> do
	text t "* 数字を入れる前に表示はクリアされている"
 ]

aboutState11 :: Page
aboutState11 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 次は画面に数字が表示されている状態で", \t -> do
	itext t 1 "- それに対する演算を行ってみる", \t -> do
	itext t 1 "*Main> arrC (* 3) 2 5", \t -> do
	itext t 1 $ show $ arrC (* (3 :: Int)) 2 5, \t -> do
	itext t 1 "*Main> arrC (+ 8) 7 23", \t -> do
	itext t 1 $ show $ arrC (+ (8 :: Int)) 7 23, \t -> do
	text t "* メモリの値は変化しない", \t -> do
	text t "* 画面の値に与えられた演算が行われている"
 ]

aboutState12 :: Page
aboutState12 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* arrCは十分に一般的に作ったので四則演算以外もできる", \t -> do
	itext t 1 "*Main> arrC even 4 7", \t -> do
	itext t 1 $ show $ arrC even (4 :: Int) 7, \t -> do
	itext t 1 "*Main> :m + Data.Char", \t -> do
	itext t 1 "*Main Data.Char> arrC chr 99 37", \t -> do
	itext t 1 $ show $ arrC chr 99 37, \t -> do
	text t "* 電卓のクリアキーは画面の値を無視して", \t -> do
	itext t 1 "- 画面の値をクリア(ユニット値, ())するので", \t -> do
	itext t 1 "*Main Data.Char> arrC (const ()) 37 8", \t -> do
	itext t 1 $ show $ arrC (const ()) (37 :: Int) 8
 ]

aboutState13 :: Page
aboutState13 = [\t -> do
	writeTopTitle t "State", \t -> do
	text t "* 今まで扱ってきた関数は共通の形を持っている", \t -> do
	text t "* その共通部分を取り出すと以下のようになる", \t -> do
	itext t 1 "a -> Int -> (b, Int)", \t -> do
	itext t 1 "- aは直前の画面の値、1つめのIntは直前のメモリの値", \t -> do
	itext t 1 "- bは直後の画面の値、2つめのIntは直後のメモリの値", \t -> do
	text t "* 今後この型を頻繁に用いるので別名をつけておこう", \t -> do
	itext t 1 "type Calc a b = a -> Int -> (b, Int)", \t -> do
	text t "* たとえば以下のようになる", \t -> do
	itext t 1 "mplus :: Calc Int ()", \t -> do
	itext t 1 "mrecall :: Calc () Int", \t -> do
	itext t 1 "arrC even :: Calc Int Bool", \t -> do
	itext t 1 "arrC chr :: Calc Int Char"
 ]

aboutState14 :: Page
aboutState14 = [\t -> do
	writeTopTitle t "State", \t -> do
	text t "* 計算の部品はそろったので次はそれを組み合わせよう", \t -> do
	text t "* 組み合わせるための関数の型は以下の形となるはずだ", \t -> do
	itext t 1 "pipeC :: Calc a b -> Calc b c -> Calc a c", \t -> do
	itext t 1 "- 画面の値をaからbにする計算と", \t -> do
	itext t 1 "- 画面の値をbからcにする計算とをつないで", \t -> do
	itext t 1 "- 画面の値をaからcにする計算をつくる", \t -> do
	text t "* 中身は以下のようになる", \t -> do
	itext t 0 "f `pipeC` g = \\x m -> let (x', m') = f x m in g x' m'", \t -> do
	text t "* let X in Yの形でXのなかで束縛した値をYのなかで使える", \t -> do
	text t "* はじめの画面とメモリの値x, mをfに与え", \t -> do
	itext t 1 "- その結果をx', m'に束縛し", \t -> do
	itext t 1 "- x', m'をgに与えている"
 ]

aboutState15 :: Page
aboutState15 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* これらをcalc.hsに書き込もう"
	itext t 0 "type Calc a b = a -> Int -> (b, Int)"
	itext t 0 "pipeC :: Calc a b -> Calc b c -> Calc a c"
	itext t 0 "f `pipeC` g = \\x m -> let (x', m') = f x m in g x' m'", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> (arrC (const 3) `pipeC` arrC (* 2)) () 23", \t -> do
	itext t 1 $ show $ (arrC (const (3 :: Int)) `pipeC` arrC (* 2)) () 23, \t -> do
	itext t 1 "*Main> (arrC (const 4) `pipeC` mplus) () 3", \t -> do
	itext t 1 $ show $ (arrC (const 4) `pipeC` mplus) () 3
 ]

type Calc a b = a -> Int -> (b, Int)

pipeC :: Calc a b -> Calc b c -> Calc a c
pipeC f g = \x m -> let (x', m') = f x m in g x' m'

aboutState16 :: Page
aboutState16 = [\t -> do
	writeTopTitle t "State", \t -> do
	text t "* 最初の例の(3 * 4 + 2 * 5) * 7を作ってみる", \t -> do
	itext t 1 "example :: () -> Int -> (Int, Int)", \t -> do
	itext t 1 "example =", \t -> do
	itext t 2 "arrC (const 3) `pipeC`", \t -> do
	itext t 2 "arrC (* 4) `pipeC`", \t -> do
	itext t 2 "mplus `pipeC`", \t -> do
	itext t 2 "arrC (const 2) `pipeC`", \t -> do
	itext t 2 "arrC (* 5) `pipeC`", \t -> do
	itext t 2 "mplus `pipeC`", \t -> do
	itext t 2 "mrecall `pipeC`", \t -> do
	itext t 2 "arrC (* 7)", \t -> do
	text t "* これをcalc.hsに書き込もう"
 ]
