import Lecture

subtitle :: String
subtitle = "トライアル 第3.7回 演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	dollar2, dollar2_2,
	flipDot, flipDot2, flipDot3,
	funCurry3, funCurry3_2, funCurry3_3, funCurry3_4,
	funUncurry3, funUncurry3_2, funUncurry3_3, funUncurry3_4,
	funOn, funOn2, funOn3,
	summary
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

dollar2 :: Page
dollar2 = [\t -> do
	writeTopTitle t "($$)"
	text t "", \t -> do
	text t "* 与えられた引数を2回関数に与える演算子($$)を定義する", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "(+) $$ 8 => 16", \t -> do
	itext t 1 "(*) $$ 3 => 9", \t -> do
	text t "* ~/lectures/lecture01/に移動しpractice.hsを作成しよう", \t -> do
	text t "* 演習1. ($$)を定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

($$) :: (a -> a -> b) -> a -> b
f $$ x = f x x

dollar2_2 :: Page
dollar2_2 = [\t -> do
	writeTopTitle t "($$)"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "($$) :: (a -> a -> b) -> a -> b"
	itext t 1 "($$) f x = f x x", \t -> do
	text t "* practice.hsに書き込み、試してみる", \t -> do
	itext t 1 "% cd ~/lectures/lecture01/", \t -> do
	itext t 1 "% ghci practice.hs", \t -> do
	itext t 1 "*Main> (+) $$ 8", \t -> do
	itext t 1 $ show $ (+) $$ (8 :: Int), \t -> do
	itext t 1 "*Main> (*) $$ 3", \t -> do
	itext t 1 $ show $ (*) $$ (3 :: Int)
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
	text t "* 演習2. 順に関数適用する関数合成演算子(>>>)を定義せよ"
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
	itext t 1 "*Main> :reload", \t -> do
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
	text t "* 演習3. curry3を定義せよ"
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
	text t "* 演習4. uncurry3を定義せよ", \t -> do
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

funOn :: Page
funOn = [\t -> do
	writeTopTitle t "on"
	text t "", \t -> do
	text t "* 「12を法にして合同」かどうかを求める関数", \t -> do
	text t "* 「12を法にして合同」は「12で割った余りが等しい」こと", \t -> do
	text t "* これを関数とすると以下のようになる", \t -> do
	itext t 1 "congruent12 :: Int -> Int -> Bool", \t -> do
	itext t 1 "congruent12 n m = (n `mod` 12) == (m `mod` 12)", \t -> do
	text t "* 文字列の長さを比較する関数", \t -> do
	itext t 1 "longerThan :: String -> String -> Bool", \t -> do
	itext t 1 "longerThan s1 s2 = length s1 > length s2", \t -> do
	text t "* これらには共通の構造がある", \t -> do
	text t "* 2つの値に同じ変換を行い、それらを2引数関数に与えている"
 ]

funOn2 :: Page
funOn2 = [\t -> do
	writeTopTitle t "on"
	text t "", \t -> do
	text t "* 共通の構造があればくくり出すことができる", \t -> do
	text t "* 2つの値に同じ変換を行い、"
	itext t 1 "それらを2引数関数に与える関数onを考える", \t -> do
	text t "* onを使った定義は以下のようになる", \t -> do
	itext t 1 "congruent12 n m = on (==) (`mod` 12) n m", \t -> do
	itext t 1 "longerThan s1 s2 = on (>) length s1 s2", \t -> do
	text t "* on op f x yとするとxとyのそれぞれにfを適用し", \t -> do
	itext t 1 "その結果をopに与える", \t -> do
	text t "* 演習5. 関数onを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

funOn3 :: Page
funOn3 = [\t -> do
	writeTopTitle t "on"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "on :: (b -> b -> c) -> (a -> b) -> a -> a -> c", \t -> do
	itext t 1 "on op f x y = f x `op` f y", \t -> do
	text t "* practice.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> on (==) (`mod` 12) 3 15", \t -> do
	itext t 1 $ show $ on (==) (`mod` 12) 3 (15 :: Int), \t -> do
	itext t 1 "*Main> on (>) length \"hello\" \"world\"", \t -> do
	itext t 1 $ show $ on (>) length "hello" "world", \t -> do
	itext t 1 "*Main> on (>) length \"sloth\" \"dog\"", \t -> do
	itext t 1 $ show $ on (>) length "sloth" "dog"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* さまざまな多相関数を見てきた", \t -> do
	text t "* 多相関数はプログラムの構造を抽象化する", \t -> do
	text t "* 演習で作成した関数は以下のようになる", \t -> do
	itext t 1 "($$), (>>>), curry3, uncurry3, on", \t -> do
	text t "* どれも具体的な演算をする関数というよりは", \t -> do
	itext t 1 "プログラムの構造を抽出した関数である", \t -> do
	text t "* プログラムのなかに共通の構造を見つけたら", \t -> do
	itext t 1 "それを関数としてくくりだすことができる"
 ]
