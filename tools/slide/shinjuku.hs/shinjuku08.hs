import Lecture

subtitle :: String
subtitle = "8. ファンクター"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, map1, fmap1, fmap2, infix1,
	summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* みんな大好きmap関数", \t -> do
	text t "* リストの要素すべてに関数を適用する", \t -> do
	text t "* リスト以外のコンテナでもmapを使いたい", \t -> do
	text t "* 「中身」に関数を適用するという枠組み", \t -> do
	text t "* それがファンクター"
	]

map1 :: Page
map1 = [ \t -> do
	writeTopTitle t "map"
	text t "", \t -> do
	text t "* リストすべてに関数を適用する", \t -> do
	itext t 1 "map :: (a -> b) -> [a] -> [b]", \t -> do
	itext t 1 "map f (x : xs) = f x : map f xs", \t -> do
	itext t 1 "map _ _ = []", \t -> do
	text t "* Maybe型は値が0または1のリストとも考えられる", \t -> do
	itext t 1 "mmap :: (a -> b) -> Maybe a -> Maybe b", \t -> do
	itext t 1 "mmap f (Just x) = Just $ f x", \t -> do
	itext t 1 "mmap _ _ = Nothing", \t -> do
	text t "* 関数mapの型宣言を書き直す", \t -> do
	itext t 1 "map :: (a -> b) -> [] a -> [] b"
	]

fmap1 :: Page
fmap1 = [ \t -> do
	writeTopTitle t "fmap"
	text t "", \t -> do
	text t "* リストとMaybe型のそれぞれのmap関数", \t -> do
	itext t 1 "map :: (a -> b) -> [] a -> [] b", \t -> do
	itext t 1 "mmap :: (a -> b) -> Maybe a -> Maybe b", \t -> do
	text t "* 共通の枠組みをとりだすと", \t -> do
	itext t 1 "(a -> b) -> f a -> f b"
	]

fmap2 :: Page
fmap2 = [ \t -> do
	writeTopTitle t "fmap"
	text t "", \t -> do
	text t "* 「中身に関数適用できる」性質を表す型クラスFunctor", \t -> do
	itext t 1 "class Functor f where", \t -> do
	itext t 2 "fmap :: (a -> b) -> f a -> f b", \t -> do
	text t "* リストをFunctorのインスタンスにする", \t -> do
	itext t 1 "instance Functor [] where", \t -> do
	itext t 2 "fmap = map", \t -> do
	text t "* Maybe型についても同様", \t -> do
	itext t 1 "instance Functor Maybe where", \t -> do
	itext t 2 "fmap = mmap"
	]

infix1 :: Page
infix1 = [ \t -> do
	writeTopTitle t "fmap"
	text t "", \t -> do
	text t "* 中置記法で書くと「関数適用」という感じが出る", \t -> do
	itext t 1 "(* 2) `fmap` [3, 4, 5]", \t -> do
	itext t 1 "(+ 5) `fmap` Just 8", \t -> do
	text t "* Data.Applicativeモジュールによりそれらしい演算子がある", \t -> do
	itext t 1 "(* 2) <$> [3, 4, 5]", \t -> do
	itext t 1 "(+ 5) <$> Just 8"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 中身に関数適用できる性質を表す型クラスFunctor"
	]
