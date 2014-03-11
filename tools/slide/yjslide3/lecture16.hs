import Data.Char

import Lecture

subtitle :: String
subtitle = "第16回 モナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, letin, aboutNewtype,
	aboutField,
	aboutMaybe, aboutMaybe2, aboutMaybe3, aboutMaybe4, aboutMaybe5,
	aboutMaybe6, aboutMaybe7, aboutMaybe8, aboutMaybe9, aboutMaybe10,
	aboutMaybe11, aboutMaybe12, aboutMaybe13, aboutMaybe14,
	aboutMaybeSummary,
	aboutState, aboutState2, aboutState3, aboutState4, aboutState5,
	aboutState6, aboutState7, aboutState8, aboutState9, aboutState10,
	aboutState11, aboutState12, aboutState13, aboutState14, aboutState15,
	aboutState16, aboutState17, aboutState18, aboutState19, aboutState20,
	aboutState21, aboutState22, aboutState23, aboutState24, aboutState25,
	aboutStateSummary,
	maybeState,
	aboutMonad, aboutMonad2, aboutMonad3, aboutMonad4,
	monadClass, maybeMonad, maybeMonad2, maybeMonad3, maybeMonad4,
	stateMonad, stateMonad3, stateMonad4, stateMonad5,
	stateMonad6, stateMonad7, stateMonad8, stateMonad9, stateMonad10,
	stateMonad11, stateMonad12, stateMonad13,
	summary
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

letin :: Page
letin = [\t -> do
	writeTopTitle t "新たに必要になる構文"
	text t "", \t -> do
	text t "* let [定義] in [表現]という形の構文がある", \t -> do
	text t "* [定義]中で定義された変数は[表現]のなかで使える", \t -> do
	text t "* 全体の値は[表現]によって表される", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> let x = 8 in x * x", \t -> do
	itext t 1 "64", \t -> do
	itext t 1 "Prelude> x", \t -> do
	itext t 1 ""
	itext t 1 "<interactive>:3:1: Not in scope: `x'"
 ]

aboutNewtype :: Page
aboutNewtype = [\t -> do
	writeTopTitle t "新たに必要になる構文"
	text t "", \t -> do
	text t "* newtypeという構文がある", \t -> do
	text t "* 使いかたとしてはdata構文とほとんど同じである", \t -> do
	itext t 1 "newtype [型構築子] [型変数1] [型変数2] ... = ", \t -> do
	itext t 2 "[値構築子] [型]", \t -> do
	text t "* dataとの違い",\t -> do
	itext t 1 "- 値構築子がひとつしか存在できず", \t -> do
	itext t 1 "- 値構築子が型をひとつしか取れない", \t -> do
	text t "* つまりnewtypeで作られる型は、他の型のラッパーとなる", \t -> do
	text t "* 内部的にはもとの型と同じ型が使われる", \t -> do
	arrowIText t 1 "効率の低下がない"
 ]

aboutField :: Page
aboutField = [\t -> do
	writeTopTitle t "新たに必要になる構文"
	text t "", \t -> do
	text t "* dataやnewtypeのフィールドに名前をつける構文がある", \t -> do
	text t "* 以下のような定義を見てみよう", \t -> do
	itext t 1 "data Human = Human String Int"
	itext t 1 "name (Human n _) = n"
	itext t 1 "age (Human _ a) = a", \t -> do
	text t "* このように書く代わりに以下のように書くことができる", \t -> do
	itext t 1 "data Human = Human { name :: String, age :: Int }", \t -> do
	text t "* フィールドを取り出す関数を用意してくれるということ"
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
	text t "* この変換を行う関数を作ろう", \t -> do
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
	itext t (-1) "* 前回の講義を思い出してみよう", \t -> do
	itext t (-1) "* 引数の型と結果の型の両方に'a ->'があるので", \t -> do
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
	text t "* この形だと、lowerToCode cの結果にnを束縛し", \t -> do
	itext t 1 "- evenDiv2 nの結果にn'を束縛し", \t -> do
	itext t 1 "- mul3 n'の値を返す、と読める", \t -> do
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
	itext t 1 "3 * 4 M+ C 2 * 5 M+ C MR * 7", \t -> do
	itext t 1 "- 3 * 4を計算しメモリに足す", \t -> do
	itext t 1 "- 表示をクリアする", \t -> do
	itext t 1 "- 2 * 5を計算しメモリに足す", \t -> do
	itext t 1 "- 表示をクリアする", \t -> do
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
	text t "* let X in Yの形でXのなかで束縛した変数をYのなかで使える", \t -> do
	text t "* はじめの画面の値xとメモリの値mをfに与え", \t -> do
	itext t 1 "- その結果をx', m'に束縛し", \t -> do
	itext t 1 "- x', m'をgに与えている"
 ]

aboutState15 :: Page
aboutState15 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* これらをcalc.hsに書き込もう", \t -> do
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
	itext t 1 "example :: Calc () Int", \t -> do
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

example :: Calc () Int
example =
	arrC (const 3) `pipeC`
	arrC (* 4) `pipeC`
	mplus `pipeC`
	arrC (const 2) `pipeC`
	arrC (* 5) `pipeC`
	mplus `pipeC`
	mrecall `pipeC`
	arrC (* 7)

aboutState17 :: Page
aboutState17 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> example () 0", \t -> do
	itext t 1 $ show $ example () 0, \t -> do
	text t "* 初期状態は", \t -> do
	itext t 1 "- 画面はクリア(ユニット値, ())されていてメモリは0", \t -> do
	text t "* それを引数として与えている"
 ]

aboutState18 :: Page
aboutState18 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* Calc型を見てみる", \t -> do
	itext t 1 "type Calc a b = a -> Int -> (b, Int)", \t -> do
	text t "* State型を作ると", \t -> do
	itext t 1 "type State b = Int -> (b, Int)", \t -> do
	itext t 1 "type Calc a b = a -> State b", \t -> do
	text t "* a -> bとa -> State bを比較してみる", \t -> do
	itext t 1 "- a -> bは画面の値の変化", \t -> do
	itext t 1 "- a -> State bは画面とメモリの変化", \t -> do
	itext t 1 "- 画面の値の変化にメモリの値の変化が追加されている"
 ]

aboutState19 :: Page
aboutState19 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* pipeCの型をState型を使って書き換えてみる", \t -> do
	itext t 0 "type State a = Int -> (a, Int)", \t -> do
	itext t 0 "pipeC ::"
	itext t 1 "(a -> State b) -> (b -> State c) -> (a -> State c)", \t -> do
	text t "* 画面とメモリを変化させる関数をつないでいる", \t -> do
	text t "* 画面の変化については明示的に示されているが", \t -> do
	itext t 1 "- メモリの変化についてはState型に隠されている", \t -> do
	text t "* pipeCを見ると1つめと2つめのa型は同じ値なので消せる", \t -> do
	itext t 1 "bindC :: State b -> (b -> State c) -> State c"
 ]

aboutState20 :: Page
aboutState20 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* bindCを定義する", \t -> do
	itext t 1 "bindC :: State a -> (a -> State b) -> State b", \t -> do
	itext t 1 "bindC f g = \\m -> let (x, m') = f m in g x m'", \t -> do
	itext t 1 "- fに状態mを与え結果の値と状態をgに与えている", \t -> do
	text t "* 同様にarrCも以下のようにできる", \t -> do
	itext t 1 "arrC :: (a -> b) -> (a -> State b)", \t -> do
	arrowIText t 1 "retC :: b -> State b", \t -> do
	itext t 1 "retC x = \\m -> (x, m)", \t -> do
	itext t 1 "- 状態は変化させずに値xを返す"
 ]

aboutState21 :: Page
aboutState21 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* これらをcalc.hsに書き込もう", \t -> do
	itext t 1 "type State a = Int -> (a, Int)", \t -> do
	itext t 1 "bindC :: State a -> (a -> State b) -> State b", \t -> do
	itext t 1 "bindC f g = \\m -> let (x, m') = f m in f x m'", \t -> do
	itext t 1 "retC :: a -> State a", \t -> do
	itext t 1 "retC x = \\m -> (x, m)"
 ]

type State a = Int -> (a, Int)

bindC :: State a -> (a -> State b) -> State b
bindC m f = \s -> let (x, s') = m s in f x s'

retC :: a -> State a
retC x = \s -> (x, s)

example' :: State Int
example' =
	retC 3 `bindC`
	(retC . (* 4)) `bindC`
	mplus `bindC` \_ ->
	retC 2 `bindC`
	(retC . (* 5)) `bindC`
	mplus `bindC`
	mrecall `bindC`
	(retC . (* 7))

aboutState22 :: Page
aboutState22 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 最初の例(3 * 4 + 2 * 5) * 7をbindC, retCで書いてみる", \t -> do
	itext t 1 "example' :: State Int", \t -> do
	itext t 1 "example' =", \t -> do
	itext t 2 "retC 3 `bindC`", \t -> do
	itext t 2 "(retC . (* 4)) `bindC`", \t -> do
	itext t 2 "mplus `bindC`", \t -> do
	itext t 2 "const (retC 2) `bindC`", \t -> do
	itext t 2 "(retC . (* 5)) `bindC`", \t -> do
	itext t 2 "mplus `bindC`", \t -> do
	itext t 2 "mrecall `bindC`", \t -> do
	itext t 2 "(retC . (* 7))", \t -> do
	text t "* これをcalc.hsに書き込もう"
 ]

aboutState23 :: Page
aboutState23 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> example' 0", \t -> do
	itext t 1 $ show $ example' 0
 ]

example'' :: State Int
example'' =
	retC 3 `bindC` \x ->
	retC (x * 4) `bindC` \y ->
	mplus y `bindC` \_ ->
	retC 2 `bindC` \z ->
	retC (z * 5) `bindC` \w ->
	mplus w `bindC` \_ ->
	mrecall () `bindC` \v ->
	retC (v * 7)

aboutState24 :: Page
aboutState24 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* 同じことを以下のように書くこともできる", \t -> do
	text t "* calc.hsに書き込もう", \t -> do
	itext t 1 "example'' =", \t -> do
	itext t 2 "retC 3 `bindC` \\x ->", \t -> do
	itext t 2 "retC (x * 4) `bindC` \\y ->", \t -> do
	itext t 2 "mplus y `bindC` \\_ ->", \t -> do
	itext t 2 "retC 2 `bindC` \\z ->", \t -> do
	itext t 2 "retC (z * 5) `bindC` \\w ->", \t -> do
	itext t 2 "mplus w `bindC` \\_ ->", \t -> do
	itext t 2 "mrecall () `bindC` \\v ->", \t -> do
	itext t 2 "retC (v * 7)"
 ]

aboutState25 :: Page
aboutState25 = [\t -> do
	writeTopTitle t "State"
	text t "", \t -> do
	text t "* これは以下のように読むことができる", \t -> do
	itext t 1 "- retC 3で返る値でxを束縛し", \t -> do
	itext t 1 "- retC (x * 4)で返る値でyを束縛し", \t -> do
	itext t 1 "- mplus yでyの値を状態に足し返り値は捨て", \t -> do
	itext t 1 "- retC 2で返る値でzを束縛し", \t -> do
	itext t 1 "- retC (z * 5)で返る値でwを束縛し", \t -> do
	itext t 1 "- mplus wでwの値を状態に足し返り値は捨て", \t -> do
	itext t 1 "- mrecall ()で状態の値を呼び出し、vを束縛し", \t -> do
	itext t 1 "- retC (v * 7)の値を返す"
 ]

aboutStateSummary :: Page
aboutStateSummary = [\t -> do
	writeTopTitle t "State(まとめ)"
	text t "", \t -> do
	text t "* メモリ付き電卓の例を見た", \t -> do
	text t "* 画面の値とメモリの値のペアを次々と変換していく", \t -> do
	text t "* 画面の値にだけ注目し、メモリの値を隠すことができた", \t -> do
	text t "* 以下の関数で変換を部品としてつないでいくことができる", \t -> do
	itext t 0 "pipeC ::"
	itext t 1 "(a -> State b) -> (b -> State c) -> (a -> State c)", \t -> do
	itext t 0 "arrC :: (a -> b) -> (a -> State b)", \t -> do
	text t "* これは以下のように簡略化できる", \t -> do
	itext t 1 "bindC :: State a -> (a -> State b) -> State c", \t -> do
	itext t 1 "retC :: a -> State a"
 ]

maybeState :: Page
maybeState = [\t -> do
	writeTopTitle t "MaybeとState"
	text t "", \t -> do
	text t "* Maybeをつなぐときに使った関数", \t -> do
	itext t 1 "retM :: a -> Maybe a"
	itext t 1 "bindM :: Maybe a -> (a -> Maybe b) -> Maybe b", \t -> do
	text t "* Stateをつなぐときに使った関数", \t -> do
	itext t 1 "retC :: a -> State a"
	itext t 1 "bindC :: State a -> (a -> State b) -> State b", \t -> do
	text t "* これらの型はMaybeとStateを置き換えただけになっている", \t -> do
	text t "* 共通する構造を抽出すると以下のようになる", \t -> do
	itext t 1 "ret :: a -> m a", \t -> do
	itext t 1 "bind :: m a -> (a -> m b) -> m b"
 ]

aboutMonad :: Page
aboutMonad = [\t -> do
	writeTopTitle t "モナド"
	text t "", \t -> do
	text t "* この2つの関数を持つ型mをモナドと呼ぶ", \t -> do
	itext t 1 "ret :: a -> m a", \t -> do
	itext t 1 "bind :: m a -> (a -> m b) -> m b", \t -> do
	text t "* これらは以下の関数の簡略化したものと考えて良い", \t -> do
	itext t 1 "arr :: (a -> b) -> (a -> m b)", \t -> do
	itext t 1 "pipe :: (a -> m b) -> (b -> m c) -> (a -> m c)", \t -> do
	text t "* モナドとするには以下の法則を満たす必要がある", \t -> do
	itext t 0.5 "1. ret `pipe` fはfと同じ", \t -> do
	itext t 0.5 "2. f `pipe` retはfと同じ", \t -> do
	itext t 0.5 "3. (f `pipe` g) `pipe` hとf `pipe` (g `pipe` h)は同じ", \t -> do
	text t "* これをモナド則と呼ぶ"
 ]

aboutMonad2 :: Page
aboutMonad2 = [\t -> do
	writeTopTitle t "モナド"
	text t "", \t -> do
	text t "* モナド則は簡単に言うと以下のことを言っている", \t -> do
	itext t 1 "- retは値を包み込むだけでそれ以外のことをしない", \t -> do
	itext t 1 "- 変換関数を左結合にしても右結合にしても同じ", \t -> do
	text t "* retを空文字列とし関数を文字列として見るとわかりやすい", \t -> do
	itext t 1 "\"\" ++ \"hello\" == \"hello\"", \t -> do
	itext t 1 "\"hello\" ++ \"\" == \"hello\"", \t -> do
	itext t 1 "(\"hello\" ++ \"my\") ++ \"friend\" ==", \t -> do
	itext t 2 "\"hello\" ++ (\"my\" ++ \"frind\")"
 ]

aboutMonad3 :: Page
aboutMonad3 = [\t -> do
	writeTopTitle t "モナド"
	text t "", \t -> do
	text t "* 何であれ以下の型の関数が存在し", \t -> do
	itext t 1 "a -> m a"
	itext t 1 "m a -> (a -> m b) -> m b", \t -> do
	text t "* それがモナド則を満たしさえすれば、型mはモナドである", \t -> do
	text t "* MaybeやStateはモナドである", \t -> do
	text t "* 中身が何であれ関係なくすべてモナドである", \t -> do
	text t "* 「モナド」とは内容ではなく形式である", \t -> do
	text t "* MaybeとStateのあいだにはほとんど共通点はない", \t -> do
	itext t 1 "- ただモナドという形式を満たすというだけ"
 ]

aboutMonad4 :: Page
aboutMonad4 = [\t -> do
	writeTopTitle t "モナド"
	text t "", \t -> do
	text t "* 同じことだが、よりイメージしやすいモナド関数の型", \t -> do
	itext t 1 "(a -> b) -> (a -> m b)", \t -> do
	itext t 1 "(a -> m b) -> (b -> m c) -> (a -> m c)", \t -> do
	text t "* 言葉で言うとこうなる", \t -> do
	itext t 1 "- 普通の関数を「値をモナドにする関数」に変換できる", \t -> do
	itext t 1 "- 「値をモナドにする関数」同士をつなぐことができる"
 ]

monadClass :: Page
monadClass = [\t -> do
	writeTopTitle t "Monadクラス"
	text t "", \t -> do
	text t "* 値を比較できるもののためにEqクラスが用意されている", \t -> do
	text t "* 同様にモナドに対してはMonadクラスが用意されている", \t -> do
	text t "* クラス定義は以下のようになっている", \t -> do
	itext t 1 "class Monad m where", \t -> do
	itext t 2 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	itext t 2 "return :: a -> m a", \t -> do
	text t "* つまり(>>=)とreturnを定義してやれば", \t -> do
	itext t 1 "- Monadのインスタンスにすることができる"
 ]

maybeMonad :: Page
maybeMonad = [\t -> do
	writeTopTitle t "Maybeモナド"
	text t "", \t -> do
	text t "* Maybe型はデフォルトでモナドクラスのインスタンスである", \t -> do
	text t "* instance宣言は以下のようになっている", \t -> do
	itext t 1 "instance Monad Maybe where", \t -> do
	itext t 2 "Nothing >>= _ = Nothing", \t -> do
	itext t 2 "Just x >>= f = f x", \t -> do
	itext t 2 "return = Just"
 ]

lowerToCodeDiv4' :: Char -> Maybe Int
lowerToCodeDiv4' c = lowerToCode c >>= evenDiv2 >>= evenDiv2

maybeMonad2 :: Page
maybeMonad2 = [\t -> do
	writeTopTitle t "Maybeモナド"
	text t "", \t -> 
	text t "* maybe.hsに以下を書き込んでみよう", \t -> do
	itext t 1 "lowerToCodeDiv4' :: Char -> Maybe Int", \t -> do
	itext t 1 "lowerToCodeDiv4' ="
	itext t 2 "lowerToCode c >>= evenDiv2 >>= evenDiv2", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :load maybe.hs", \t -> do
	itext t 1 "*Main> lowerToCodeDiv4' 'n'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv4' 'n', \t -> do
	itext t 1 "*Main> lowerToCodeDiv4' 'p'", \t -> do
	itext t 1 $ show $ lowerToCodeDiv4' 'p'
 ]

maybeMonad3 :: Page
maybeMonad3 = [\t -> do
	writeTopTitle t "Maybeモナド"
	text t "", \t -> do
	text t "* 同じことだが以下のように書くこともできる", \t -> do
	itext t 1 "lowerToCodeDiv4'' c =", \t -> do
	itext t 2 "lowerToCode c >>= \\n ->"
	itext t 2 "evenDiv2 n >>= \\n' ->"
	itext t 2 "evenDiv2 n'", \t -> do
	text t "* これは以下のように読める", \t -> do
	itext t 1 "- lowerToCode cが返す値でnを束縛して", \t -> do
	itext t 1 "- evenDiv2 nが返す値でn'を束縛して", \t -> do
	itext t 1 "- evenDiv2 n'の値を返す"
 ]

maybeMonad4 :: Page
maybeMonad4 = [\t -> do
	writeTopTitle t "Maybeモナド"
	text t "", \t -> do
	text t "* do記法という構文糖がある", \t -> do
	text t "* それを使うと同じことが以下のように書ける", \t -> do
	itext t 1 "lowerToCodeDiv4''' c = do"
	itext t 2 "n <- lowerToCode c"
	itext t 2 "n' <- evenDiv2 n"
	itext t 2 "evenDiv2 n'", \t -> do
	text t "* doという識別子で始める", \t -> do
	text t "* それぞれの行で以下の変換が行われる", \t -> do
	itext t 1 "[変数] <- [表現]", \t -> do
	arrowIText t 1 "[表現] >>= \\[変数] ->"
 ]

stateMonad :: Page
stateMonad = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* calc.hsで定義したStateは単なる別名", \t -> do
	itext t 1 "type State a = Int -> (a, Int)", \t -> do
	text t "* 別名に対してはinstance宣言はできない", \t -> do
	text t "* 別名をつける代わりにnewtypeを使うことができる", \t -> do
	text t "* newtypeは内部的にはtypeと同じだが", \t -> do
	itext t 1 "- 使いかたとしてはdataと同様に使える", \t -> do
	text t "* dataを使っても使いかたはほとんど同じだが", \t -> do
	itext t 1 "- newtypeを使ったほうが実行効率は良くなる", \t -> do
	text t "* newtypeを使いMonadクラスのインスタンスとする", \t -> do
	text t "* state.hsに書き込んでいこう"
 ]

{-
stateMonad2 :: Page
stateMonad2 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* ここでひとつ省略記法を紹介する", \t -> do
	text t "* dataやnewtype宣言のなかでフィールド名を指定できる", \t -> do
	itext t 1 "data Human = Human String Int", \t -> do
	itext t 1 "name (Human n _) = n", \t -> do
	itext t 1 "age (Human _ a) = a", \t -> do
	text t "* 上のように書く代わりに以下のように書ける", \t -> do
	itext t 1 "data Human = Human { name :: String, age :: Int }", \t -> do
	text t "* フィールドを取り出す関数を用意してくれる"
 ]
 -}

stateMonad3 :: Page
stateMonad3 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* State型を定義しよう。state.hsに以下を書き込む", \t -> do
	itext t 0 "newtype State a = State { runState :: Int -> (a, Int) }", \t -> do
	text t "* この定義は以下の2つの定義とだいたい同じ", \t -> do
	itext t 1 "newtype State a = State (Int -> (a, Int))", \t -> do
	itext t 1 "runState (State st) = st", \t -> do
	text t "* 簡単に言えばStateで服を着せて", \t -> do
	itext t 1 "- runStateで服を脱がせるということ"
 ]

newtype State' a = State { runState :: Int -> (a, Int) }

instance Monad State' where
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'
	return x = State $ \s -> (x, s)

stateMonad4 :: Page
stateMonad4 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* Monadクラスのインスタンスにする", \t -> do
	itext t 0 "instance Monad State where", \t -> do
	itext t 1 "State m >>= f = State $ \\s ->"
	itext t 2 "let (x, s') = m s in runState (f x) s'", \t -> do
	itext t 1 "return x = State $ \\s -> (x, s)", \t -> do
	text t "* 複雑に見えるがStateやrunStateを消して考えれば良い", \t -> do
	itext t 1 "- それらは服を着せたり脱がせたりしているだけ", \t -> do
	text t "* state.hsに書き込もう"
 ]

stateMonad5 :: Page
stateMonad5 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* mplusはより一般的な形に直せる", \t -> do
	text t "* put, get関数を定義してみよう", \t -> do
	itext t 1 "put :: Int -> State ()", \t -> do
	itext t 1 "put s = State $ \\_ -> ((), s)", \t -> do
	itext t 1 "get :: State Int", \t -> do
	itext t 1 "get = State $ \\s -> (s, s)", \t -> do
	text t "* putは引数の値で「状態」を置き換える", \t -> do
	text t "* getは「状態」を「画面」にコピーする", \t -> do
	itext t 1 "- mrecallと同じ", \t -> do
	text t "putとgetの定義をstate.hsに書き込もう"
 ]

stateMonad6 :: Page
stateMonad6 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* putとgetを使ってmodifyが定義できる", \t -> do
	itext t 1 "modify :: (Int -> Int) -> State ()", \t -> do
	itext t 1 "modify f = get >>= put . f", \t -> do
	text t "* mplusはmodifyを使って定義できる", \t -> do
	itext t 1 "mplus :: Int -> State ()", \t -> do
	itext t 1 "mplus x = modify (+ x)", \t -> do
	text t "* state.hsに書き込もう"
 ]

put :: Int -> State' ()
put s = State $ \_ -> ((), s)

get :: State' Int
get = State $ \s -> (s, s)

modify :: (Int -> Int) -> State' ()
modify f = get >>= put . f

mplus' :: Int -> State' ()
mplus' x = modify (+ x)

stateMonad7 :: Page
stateMonad7 = [\t -> do
	writeTopTitle t "Stateモナド", \t -> do
	text t "* 最初の例(3 * 4 + 2 * 5) * 7を作ってみよう", \t -> do
	text t "* state.hsに書き込もう", \t -> do
	itext t 1 "example :: State Int", \t -> do
	itext t 1 "example =", \t -> do
	itext t 2 "return 3 >>=", \t -> do
	itext t 2 "return . (* 4) >>=", \t -> do
	itext t 2 "mplus >>=", \t -> do
	itext t 2 "const (return 2) >>=", \t -> do
	itext t 2 "return . (* 5) >>=", \t -> do
	itext t 2 "mplus >>=", \t -> do
	itext t 2 "const get >>=", \t -> do
	itext t 2 "return . (* 7)"
 ]

example''' :: State' Int
example''' =
	return 3 >>=
	return . (* 4) >>=
	mplus' >>=
	const (return 2) >>=
	return . (* 5) >>=
	mplus' >>=
	const get >>=
	return . (* 7)

stateMonad8 :: Page
stateMonad8 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :load state.hs", \t -> do
	itext t 1 "*Main> runState example 0", \t -> do
	itext t 1 $ show $ runState example''' 0, \t -> do
	text t "* runStateで服を脱がせたうえで", \t -> do
	itext t 1 "- 初期値の0を与えている"
 ]

stateMonad9 :: Page
stateMonad9 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* exampleの定義のなかで2ヶ所にconstが出てきた", \t -> do
	text t "* これは直前の計算の返り値を使わないということ", \t -> do
	text t "* 次の計算に返り値を渡さない場合", \t -> do
	itext t 1 "- (>>=)の代わりに(>>)を使うと良い", \t -> do
	itext t 1 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	itext t 1 "(>>) :: m a -> m b -> m b", \t -> do
	text t "* (>>)の定義は以下のようになる", \t -> do
	itext t 1 "m1 >> m2 = m1 >>= const m2", \t -> do
	text t "* 以下のように書いても同じこと", \t -> do
	itext t 1 "m1 >> m2 = m1 >>= \\_ -> m2"
 ]

stateMonad10 :: Page
stateMonad10 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* (>>)を使ってexampleを書き換えると以下のようになる", \t -> do
	itext t 1 "example' =", \t -> do
	itext t 2 "return 3 >>=", \t -> do
	itext t 2 "return . (* 4) >>=", \t -> do
	itext t 2 "mplus >>", \t -> do
	itext t 2 "return 2 >>=", \t -> do
	itext t 2 "return . (* 5) >>=", \t -> do
	itext t 2 "mplus >>", \t -> do
	itext t 2 "get >>=", \t -> do
	itext t 2 "return . (* 7)"
 ]

stateMonad11 :: Page
stateMonad11 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* 明示的な局所変数を使った書き換え", \t -> do
	itext t 1 "example'' =", \t -> do
	itext t 2 "return 3 >>= \\x ->", \t -> do
	itext t 2 "return (x * 4) >>= \\y ->", \t -> do
	itext t 2 "mplus y >>", \t -> do
	itext t 2 "return 2 >>= \\z ->", \t -> do
	itext t 2 "return (z * 5) >>= \\w ->", \t -> do
	itext t 2 "mplus w >>", \t -> do
	itext t 2 "get >>= \\v ->", \t -> do
	itext t 2 "return (v * 7)"
 ]

stateMonad12 :: Page
stateMonad12 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* do記法を使った書き換え", \t -> do
	itext t 1 "example''' = do", \t -> do
	itext t 2 "x <- return 3", \t -> do
	itext t 2 "y <- return $ x * 4", \t -> do
	itext t 2 "mplus y", \t -> do
	itext t 2 "z <- return 2", \t -> do
	itext t 2 "w <- return $ z * 5", \t -> do
	itext t 2 "mplus w", \t -> do
	itext t 2 "v <- get", \t -> do
	itext t 2 "return $ v * 7"
 ]

stateMonad13 :: Page
stateMonad13 = [\t -> do
	writeTopTitle t "Stateモナド", \t -> do
	text t "* [変数] <- return [値]という形が出てきたが", \t -> do
	itext t 1 "- この形にはlet [変数] = [値]という構文糖が使える", \t -> do
	itext t 1 "- また、連続するletはひとつにまとめられる", \t -> do
	itext t 1 "example''' = do", \t -> do
	itext t 2 "let"
	preLine t
	itext t 3 "x = 3", \t -> do
	itext t 3 "y = x * 4", \t -> do
	itext t 2 "mplus y", \t -> do
	itext t 2 "let"
	preLine t
	itext t 3 "z = 2", \t -> do
	itext t 3 "w = z * 5", \t -> do
	itext t 2 "mplus w", \t -> do
	itext t 2 "v <- get", \t -> do
	itext t 2 "return $ v * 7"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* (a -> m b)の形の関数を結合する規則が存在し", \t -> do
	itext t 1 "- それがモナド則を満たせばmはモナドである", \t -> do
	text t "* 以下の型の関数が必要である", \t -> do
	itext t 1 "m a -> (a -> m b) -> m b", \t -> do
	itext t 1 "a -> m a", \t -> do
	text t "* 条件を満たせば何でもモナド", \t -> do
	text t "* 今回はMaybeモナドとStateモナドを見た", \t -> do
	text t "* その2つは中身は大きく異なるがともにモナドである", \t -> do
	text t "* Monadクラスが用意されている", \t -> do
	text t "* Monadクラスのインスタンスにするとdo記法が使える"
 ]
