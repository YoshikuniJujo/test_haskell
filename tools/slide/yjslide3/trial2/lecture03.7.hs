import Lecture

subtitle :: String
subtitle = "トライアル 第3.7回 演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, flipDot, flipDot2, flipDot3
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今日は関数について学んだ", \t -> do
	text t "* 高階関数や多相関数は慣れるまで理解するのが難しい", \t -> do
	text t "* その関数が何をやっているのかわからないときは", \t -> do
	itext t 1 "適当な引数に対して展開してみるのもひとつの方法", \t -> do
	text t "* 「演習」では実際にいくつかの関数を定義してみよう"
 ]

flipDot :: Page
flipDot = [\t -> do
	writeTopTitle t "(>>>)"
	text t "", \t -> do
	text t "* 関数合成演算子(.)について学んだ", \t -> do
	text t "* (.)はf (g x)を(f . g) xに変換する", \t -> do
	text t "* 関数の適用の順番としては第2引数->第1引数となる", \t -> do
	text t "* 人間の感覚に近い第1引数->第2引数の順となる関数を考える", \t -> do
	text t "* 例えば「3足して2かける」を以下のように書けるようにする", \t -> do
	itext t 1 "(+ 3) >>> (* 2)", \t -> do
	text t "* ~/lectures/lecture01/に移動しpractice.hsを作成しよう", \t -> do
	text t "* 演習1. 順に関数適用する関数合成演算子(>>>)を定義せよ"
	itext t 1 "(1分)"
 ]

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

flipDot2 :: Page
flipDot2 = [\t -> do
	writeTopTitle t "(>>>)"
	text t "", \t -> do
	text t "* 以下のようになるだろう", \t -> do
	itext t 1 "(>>>) :: (a -> b) -> (b -> c) -> a -> c", \t -> do
	itext t 1 "(>>>) f g x = g (f x)", \t -> do
	text t "* 「関数合成してますよ」という意図を明確にしたければ", \t -> do
	itext t 1 "(>>>) :: (a -> b) -> (b -> c) -> (a -> c)", \t -> do
	itext t 1 "f >>> g = \\x -> g (f x)", \t -> do
	text t "* 別解としては以下も考えられる", \t -> do
	itext t 1 "(>>>) = flip (.)", \t -> do
	text t "* practice.hsに書き込もう"
 ]

flipDot3 :: Page
flipDot3 = [\t -> do
	writeTopTitle t "(>>>)"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "% cd ~/lectures/lecture01/", \t -> do
	itext t 1 "% ghci practice.hs", \t -> do
	itext t 1 "*Main> (+ 3) >>> (* 2) $ 4", \t -> do
	itext t 1 $ show $ (+ 3) >>> (* 2) $ (4 :: Int)
 ]
