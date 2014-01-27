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
	aboutMaybeSummary
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
