import Lecture

subtitle :: String
subtitle = "第22回 ScopedTypeVariables拡張"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	typeVarScope, typeVarScope2, typeVarScope3, typeVarScope4, typeVarScope5,
	typeVarScopeSummary,
	patternSignature, patternSignature2, patternSignature3,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ScopedTypeVariablesという言語拡張がある", \t -> do
	text t "* 型変数のスコープを広げる", \t -> do
	text t "* パターンに型シグネチャをつけられる", \t -> do
	text t "* {-# LANGUAGE ScopedTypeVariables #-}とする必要がある"
 ]

typeVarScope :: Page
typeVarScope = [\t -> do
	writeTopTitle t "型変数のスコープ"
	text t "", \t -> do
	text t "* 以下の関数を見てみよう", \t -> do
	itext t 1 "twice :: [a] -> [a]"
	itext t 1 "twice xs = ys"
	itext t 2 "where"
	itext t 2 "ys = xs ++ xs", \t -> do
	itext t 1 "> twice \"hello\""
	itext t 1 "\"hellohello\"", \t -> do
	text t "* 何の変哲もない関数だ"
 ]

typeVarScope2 :: Page
typeVarScope2 = [\t -> do
	writeTopTitle t "型変数のスコープ"
	text t "", \t -> do
	text t "* ysに型シグネチャをつけてみる", \t -> do
	itext t 1 "twice :: [a] -> [a]"
	itext t 1 "twice xs = ys"
	itext t 2 "where"
	itext t 2 "ys :: [a]"
	itext t 2 "ys = xs ++ xs", \t -> do
	arrowIText t 1 "エラーになる", \t -> do
	itext t 1 "Couldn't match type `a' with `a1' ...", \t -> do
	text t "* twice :: [a] -> [a]のaとys :: [a]のaは別物"
 ]

typeVarScope3 :: Page
typeVarScope3 = [\t -> do
	writeTopTitle t "型変数のスコープ"
	text t "", \t -> do
	text t "* 例えば以下の例を見てみよう", \t -> do
	itext t 1 "(\\x -> x + 1) 8 + (\\x -> x + 2) 9", \t -> do
	text t "* この場合左の括弧内のxと右の括弧内のxとは別物である", \t -> do
	text t "* つまりこの2つは別々のスコープにあるので", \t -> do
	itext t 1 "- 名前が同じでも、別の変数である", \t -> do
	text t "* 同じ意味でtwice :: [a] -> [a]のaとys :: [a]のaは別物", \t -> do
	text t "* つまり型変数のスコープは型シグネチャ内ということ"
 ]

typeVarScope4 :: Page
typeVarScope4 = [\t -> do
	writeTopTitle t "型変数のスコープ"
	text t "", \t -> do
	text t "* 型宣言で定義された型変数が関数本体内で必要な例", \t -> do
	itext t 1 "one :: (Num a, Show a) => a -> String"
	itext t 1 "one x = show (1 :: a)", \t -> do
	text t "* 引数で与えられた数と同じ型での1を文字列で得る関数", \t -> do
	itext t 1 "> one 3"
	itext t 1 "\"1\"", \t -> do
	itext t 1 "> one 3.0"
	itext t 1 "\"1.0\"", \t -> do
	itext t 1 "> one (3 % 4)"
	itext t 1 "\"1 % 1\"", \t -> do
	text t "* しかし、これは型チェックを通らない"
 ]

typeVarScope5 :: Page
typeVarScope5 = [\t -> do
	writeTopTitle t "型変数のスコープ"
	text t "", \t -> do
	text t "* 型変数のスコープを広げたい", \t -> do
	text t "* ScopedTypeVariables言語拡張を使う", \t -> do
	text t "* {-# LANGUAGE ScopedTypeVariables #-}を宣言したうえで", \t -> do
	itext t 1 "one :: forall a . (Num a, Show a) => a -> String"
	itext t 1 "one x = show (1 :: a)", \t -> do
	text t "* forall aによって明示的に型変数aを宣言している", \t -> do
	text t "* これでこの関数は動くようになる"
 ]

typeVarScopeSummary :: Page
typeVarScopeSummary = [\t -> do
	writeTopTitle t "型変数のスコープ(まとめ)"
	text t "", \t -> do
	text t "* 型変数のスコープはもともとはその型シグネチャ内", \t -> do
	text t "* 型変数のスコープを関数全体に広げることができる", \t -> do
	itext t 1 "- ScopedTypeVariables拡張を使う", \t -> do
	itext t 1 "- そのうえで、forallによって型変数を明示的に宣言"
 ]

patternSignature :: Page
patternSignature = [\t -> do
	writeTopTitle t "パターンシグネチャ"
	text t "", \t -> do
	text t "* もともとはPatternSignaturesという独立した言語拡張", \t -> do
	text t "* 現在はScopedTypeVariablesを使う", \t -> do
	itext t 1 "- 型変数をスコープに入れるという点で類似した機能", \t -> do
	itext t 1 "one (x :: a) = show (1 :: a)"
 ]

patternSignature2 :: Page
patternSignature2 = [\t -> do
	writeTopTitle t "パターンシグネチャ"
	text t "", \t -> do
	text t "* 型変数のスコープという話とは違う使いかたもできる", \t -> do
	itext t 1 "withOne :: (Num a, Show a) => (a -> a) -> String"
	itext t 1 "withOne f = show $ f 1", \t -> do
	itext t 1 "> withOne $ \\x -> x + 3 * 8"
	itext t 1 "\"25\"", \t -> do
	itext t 1 "> withOne $ \\x -> (x :: Double) + 3 * 8"
	itext t 1 "\"25.0\"", \t -> do
	itext t 1 "> withOne $ \\x -> 3 * 8"
	itext t 1 "\"24\"", \t -> do
	text t "* \"24.0\"を出力させるにはどうすればいいか"
 ]

patternSignature3 :: Page
patternSignature3 = [\t -> do
	writeTopTitle t "パターンシグネチャ"
	text t "", \t -> do
	text t "* パターンシグネチャを使えばスマートに解決できる", \t -> do
	itext t 1 "> withOne $ \\(x :: Double) -> 3 * 8"
	itext t 1 "\"24.0\""
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* ScopedTypeVariablesについて学んだ", \t -> do
	text t "* 型変数のスコープを関数全体に広げることができる", \t -> do
	text t "* ScopedTypeVariablesでパターンシグネチャが使える", \t -> do
	text t "* パターンシグネチャは型変数を導入する以外にも使える", \t -> do
	text t "* ラムダ記法を使ったある様の表現をシンプルにする"
 ]
