import Lecture

subtitle :: String
subtitle = "11. モナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	maybe1, maybe2, list1, list2, maybeList, convert, bind1, bind2,
	return1, monad, summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* モナド!モナド!モナド!", \t -> do
	text t "* Haskellと言えばモナド、モナドと言えばHaskell", \t -> do
	text t "* 謎めいた内輪だけの合言葉のようだ", \t -> do
	text t "* 何が難しいのか", \t -> do
	text t "* モナドとは中身のない枠組み", \t -> do
	text t "* 共通の枠組みを持てばまるで違うものがモナドとなる", \t -> do
	text t "* 中身がないので抽象度が高い", \t -> do
	text t "* 逆に単純すぎて「何の意味があるのか」がわかりにくい"
	]

prelude2 :: Page
prelude2 = [ \t -> do
	writeTopTitle t "注意"
	text t "", \t -> do
	text t "* 本当ならばモナド則について触れるべきだがここでは省略"
	]

maybe1 :: Page
maybe1 = [ \t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* Maybe型を返す関数は", \t -> do
	itext t 1 "「失敗するかもしれない関数」ととらえられる", \t -> do
	text t "* 「失敗するかもしれない関数」はつなぎたくなる", \t -> do
	text t "* 演算1、演算2、演算3 ...と演算をつなげて", \t -> do
	itext t 1 "「失敗」したところで全体も失敗させたくなるだろう", \t -> do
	text t "* 以下のような関数がほしくなるということだ", \t -> do
	itext t 1 "mpipe :: (a -> Maybe b) -> (b -> Maybe c)"
	itext t 5 "-> a -> Maybe c", \t -> do
	text t "* 関数mpipeがあれば", \t -> do
	itext t 1 "f `mpipe` g `mpipe` h `mpipe` ...", \t -> do
	itext t 0.5 "のように関数をつなげていける"
	]

maybe2 :: Page
maybe2 = [ \t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* 関数mpipeを定義してみよう", \t -> do
	itext t 1 "mpipe f g x = case f x of", \t -> do
	itext t 2 "Just y -> g y", \t -> do
	itext t 2 "_ -> Nothing", \t -> do
	text t "* 関数fを適用した結果がJust値なら関数gに結果をわたし", \t -> do
	itext t 1 "そうでないならNothing値としている"
	]

list1 :: Page
list1 = [ \t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* リストをかえす関数は見かたによっては", \t -> do
	itext t 1 "「複数の可能性をかえす関数」ととらえられる", \t -> do
	text t "* 「複数の可能性をかえす関数」はつなぎたくなる", \t -> do
	text t "* たとえば2通りの可能性がかえってきたらそれぞれについて", \t -> do
	itext t 1 "「複数の可能性をかえす関数」を適用したい", \t -> do
	text t "* 以下のような関数がほしくなる", \t -> do
	itext t 1 "lpipe :: (a -> [b]) -> (b -> [c]) -> a -> [c]"
	]

list2 :: Page
list2 = [ \t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* 関数lpipeを定義してみよう", \t -> do
	itext t 1 "lpipe f g x = concat $ g `map` f x", \t -> do
	text t "* f xでかえされた複数の結果のそれぞれについてgを適用し", \t -> do
	itext t 1 "結果の「リストのリスト」を「リスト」にしている"
	]

maybeList :: Page
maybeList = [ \t -> do
	writeTopTitle t "Maybe型とリスト"
	text t "", \t -> do
	text t "* Maybe型とリストのそれぞれの関数を比較する", \t -> do
	itext t 1 "mpipe :: (a -> Maybe b) -> (b -> Maybe c)", \t -> do
	itext t 5 "-> a -> Maybe c", \t -> do
	itext t 1 "lpipe :: (a -> [] b) -> (b -> [] c) -> a -> [] c", \t -> do
	text t "* 共通する枠組みは", \t -> do
	itext t 1 "pipe :: (a -> m b) -> (b -> m c) -> a -> m c"
	]

convert :: Page
convert = [ \t -> do
	writeTopTitle t "関数の変換"
	text t "", \t -> do
	text t "* 関数pipeの型を見ると", \t -> do
	itext t 1 "pipe :: (a -> m b) -> (b -> m c) -> a -> m c", \t -> do
	text t "* 引数を削除することのできる形となっている", \t -> do
	itext t 1 "(a -> X) -> Y -> a -> Z", \t -> do
	text t "* よって以下のような関数と同じものとみなせる", \t -> do
	itext t 1 "bind :: m b -> (b -> m c) -> m c", \t -> do
	text t "* 引数がすくないぶん関数bindのほうが単純だ"
	]

bind1 :: Page
bind1 = [ \t -> do
	writeTopTitle t "単純な形で"
	text t "", \t -> do
	text t "* Maybe型について単純な形で定義してみる", \t -> do
	itext t 1 "mbind :: Maybe a -> (a -> Maybe b) -> Maybe b", \t -> do
	itext t 1 "mbind (Just x) f = f x", \t -> do
	itext t 1 "mbind _ = Nothing", \t -> do
	text t "* 以下のように書きかえられる", \t -> do
	itext t 1 "f `mpipe` g `mpipe` h ...", \t -> do
	itext t 1 "\\x -> f x `mbind` g `mbind` h ..."
	]

bind2 :: Page
bind2 = [ \t -> do
	writeTopTitle t "単純な形で"
	text t "", \t -> do
	text t "* リストについても同様に", \t -> do
	itext t 1 "lbind :: [a] -> (a -> [b]) -> [b]", \t -> do
	itext t 1 "lbind xs f = concat $ map f xs"
	]

return1 :: Page
return1 = [ \t -> do
	writeTopTitle t "通常の関数"
	text t "", \t -> do
	text t "* 以下のようにつないでいくとき", \t -> do
	itext t 1 "\\x -> f x `mbind` g `mbind` h ...", \t -> do
	text t "* 関数gが通常の値をかえす関数だとするとうまく動かない", \t -> do
	text t "* 関数pureと同じように関数returnを定義してやる", \t -> do
	itext t 1 "return :: a -> m a", \t -> do
	text t "* 以下のようにできる", \t -> do
	itext t 1 "\\x -> f x `mbind` (return . g) `mbind` h ..."
	]

monad :: Page
monad = [ \t -> do
	writeTopTitle t "Monad"
	text t "", \t -> do
	text t "* Monadであるためにはreturnとbindがあればよい", \t -> do
	text t "* Haskellではbindには(>>=)という名前がついている", \t -> do
	itext t 1 "class Monad m where", \t -> do
	itext t 2 "(>>=) :: m a -> (a -> m b) -> m b", \t -> do
	itext t 2 "return :: a -> m a"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* モナドは(f :: a -> m b)と(g :: b -> m c)とをつなげて", \t -> do
	itext t 1 "(h :: a -> m c)を作れるという枠組みだ", \t -> do
	text t "* 単純な関数(fun :: a -> b)は(return . fun)とすることで", \t -> do
	itext t 1 "(a -> m b)型の関数にすることができる", \t -> do
	text t "* 特殊な値をかえす関数をつなげていく枠組みがモナドだ", \t -> do
	text t "* 中身は問題ではないモナドの形式にあてはまりさえすれば", \t -> do
	itext t 1 "それはモナドである", \t -> do
	text t "* モナドという形式を持つコンテナなどに対して", \t -> do
	itext t 1 "共通に適用できる演算がいくつもある"
	]
