import Lecture

subtitle :: String
subtitle = "トライアル 第3.7回 演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, flipDot, flipDot2, flipDot3,
	funCurry3, funCurry3_2, funCurry3_3, funCurry3_4,
	funUncurry3, funUncurry3_2, funUncurry3_3, funUncurry3_4
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

showName :: (String, Int, Bool) -> String
showName (name, age, True) = name ++ "(" ++ show age ++ ")"
showName (name, _, False) = name

bob, alice :: (String, Int, Bool)
bob = ("Bob", 25, True)
alice = ("Alice", 33, False)

funCurry3 :: Page
funCurry3 = [\t -> do
	writeTopTitle t "curry3"
	text t "", \t -> do
	text t "* 2要素タプルを2つの引数にばらす関数について見た", \t -> do
	text t "* 今度は3要素タプルを3つの引数にばらす関数を作る", \t -> do
	text t "* 次のような関数を考えよう", \t -> do
	itext t 1 "showName :: (String, Int, Bool) -> String", \t -> do
	itext t 1 "showName (name, age, True) ="
	itext t 2 "name ++ \"(\" ++ show age ++ \")\"", \t -> do
	itext t 1 "showName (name, _, False) = name", \t -> do
	text t "* これをpractice.hsに書き込む"
 ]

funCurry3_2 :: Page
funCurry3_2 = [\t -> do
	writeTopTitle t "curry3"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 $ "*Main> showName " ++ show bob, \t -> do
	itext t 1 $ show $ showName bob, \t -> do
	itext t 1 $ "*Main> showName " ++ show alice, \t -> do
	itext t 1 $ show $ showName alice
 ]

funCurry3_3 :: Page
funCurry3_3 = [\t -> do
	writeTopTitle t "curry3"
	text t "", \t -> do
	text t "* curry3は以下のように使えるものとする", \t -> do
	itext t 1 "(curry3 showName) \"Bob\" 25 True", \t -> do
	text t "* 演習2. curry3を定義せよ"
	itext t 1 "(1分)"
 ]

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

funCurry3_4 :: Page
funCurry3_4 = [\t -> do
	writeTopTitle t "curry3"
	text t "", \t -> do
	text t "* 答えは以下のようになる", \t -> do
	itext t 1 "curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d", \t -> do
	itext t 1 "curry3 f x y z = f (x, y, z)", \t -> do
	text t "* より「関数の変換」を強調したければ", \t -> do
	itext t 1 "curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)", \t -> do
	itext t 1 "curry3 f = \\x y z -> f (x, y, z)", \t -> do
	text t "* practice.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> curry3 showName \"Bob\" 25 True", \t -> do
	itext t 1 $ show $ curry3 showName "Bob" 25 True, \t -> do
	itext t 1 "*Main> curry3 showName \"Alice\" 33 False", \t -> do
	itext t 1 $ show $ curry3 showName "Alice" 33 False
 ]

myIf :: Bool -> a -> a -> a
myIf True t _ = t
myIf False _ e = e

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

funUncurry3 :: Page
funUncurry3 = [\t -> do
	writeTopTitle t "uncurry3"
	text t "", \t -> do
	text t "* 同様にばらばらの3つの引数を3要素タプルにまとめる", \t -> do
	text t "* そのような関数uncurry3を考えてみる", \t -> do
	text t "* 以下のような関数を考えてみる", \t -> do
	itext t 1 "myIf :: Bool -> a -> a -> a", \t -> do
	itext t 1 "myIf True t _ = t", \t -> do
	itext t 1 "myIf False _ e = e", \t -> do
	text t "* これをpractice.hsに保存し、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> myIf True 3 8", \t -> do
	itext t 1 $ show $ myIf True 3 (8 :: Int), \t -> do
	itext t 1 "Main> myIf False 3 8", \t -> do
	itext t 1 $ show $ myIf False 3 (8 :: Int)
 ]

funUncurry3_2 :: Page
funUncurry3_2 = [\t -> do
	writeTopTitle t "uncurry3"
	text t "", \t -> do
	text t "* 以下のように使えるuncurry3を考える", \t -> do
	itext t 1 "uncurry3 myIf (True, 3, 8)", \t -> do
	text t "* 演習3. uncurry3を定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

funUncurry3_3 :: Page
funUncurry3_3 = [\t -> do
	writeTopTitle t "uncurry3"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d", \t -> do
	itext t 1 "uncurry3 f (x, y, z) = f x y z", \t -> do
	text t "* 関数を変換するということを強調するには", \t -> do
	itext t 1 "uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)", \t -> do
	itext t 1 "uncurry3 f = \\(x, y, z) -> f x y z", \t -> do
	text t "* practice.hsに書き込む"
 ]

funUncurry3_4 :: Page
funUncurry3_4 = [\t -> do
	writeTopTitle t "uncurry3"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> uncurry3 myIf (True, 3, 8)", \t -> do
	itext t 1 $ show $ uncurry3 myIf (True, 3, 8 :: Int), \t -> do
	itext t 1 "*Main> uncurry3 myIf (False, 3, 8)", \t -> do
	itext t 1 $ show $ uncurry3 myIf (False, 3, 8 :: Int)
 ]
