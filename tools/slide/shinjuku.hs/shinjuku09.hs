import Lecture

subtitle :: String
subtitle = "9. アプリカティブファンクター"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	maybe1, maybe2, list1, list2, maybeList, pure1, pure2, applicative,
	summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 多引数関数を複数のリストに適用したいとする", \t -> do
	text t "* 多引数関数を複数のMaybe値に適用したいとする", \t -> do
	text t "* 多引数関数を複数のXに適用したい", \t -> do
	text t "* そのようなことができるとき", \t -> do
	itext t 1 "「Xはアプリカティブファンクターである」という"
	]

maybe1 :: Page
maybe1 = [ \t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* たとえば2つのMaybe値の中身同士を足し合わせたい", \t -> do
	text t "* どちらかがNothingなら結果もNothingだ", \t -> do
	text t "* fmapで(+)の第1引数部分はあたえられる", \t -> do
	itext t 1 "fmap (+) (Just 8)", \t -> do
	text t "* これは結果として以下のようになる", \t -> do
	itext t 1 "Just (8 +)", \t -> do
	text t "* コンテナのなかに関数が入っている状態だ"
	]

maybe2 :: Page
maybe2 = [ \t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* コンテナのなかに関数が入っている", \t -> do
	itext t 1 "Just (8 +)", \t -> do
	text t "* これに対してさらにMaybe型の中身の値をあたえたい", \t -> do
	text t "* コンテナのなかの関数をコンテナのなかの値に適用したい", \t -> do
	text t "* Maybe型であれば以下のようになる", \t -> do
	itext t 1 "maf :: Maybe (a -> b) -> Maybe a -> Maybe b", \t -> do
	itext t 1 "maf (Just f) (Just x) = Just $ f x", \t -> do
	itext t 1 "maf _ _ = Nothing"
	]

list1 :: Page
list1 = [ \t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* 同じことをリストで見てみよう", \t -> do
	text t "* [1, 2, 3, 4]と[10, 100, 1000]をかけあわせたい", \t -> do
	text t "* まずは(*)に[1, 2, 3, 4]をあたえる", \t -> do
	itext t 1 "fmap (*) [1, 2, 3, 4]", \t -> do
	arrowIText t 1 "[(1 *), (2 *), (3 *), (4 *)]", \t -> do
	text t "* リストのなかの関数をリストのなかの値に適用したい", \t -> do
	itext t 1 "laf :: [a -> b] -> [a] -> [b]", \t -> do
	itext t 1 "laf (f : fs) xs = map f xs ++ laf fs xs", \t -> do
	itext t 1 "laf _ _ = []"
	]

list2 :: Page
list2 = [ \t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* [1, 2, 3, 4]と[10, 100, 1000]をかけあわせる", \t -> do
	itext t 1 "laf (fmap (*) [1, 2, 3, 4]) [10, 100, 1000]"
	arrowIText t 1 "[10, 20, 30, 40, 100, 200, 300, 400, 1000,"
	itext t 1 "2000, 3000, 40000]", \t -> do
	text t "* 中置記法にすると読みやすくなる", \t -> do
	itext t 1 "(*) `fmap` [1, 2, 3, 4] `laf` [10, 100, 1000]", \t -> do
	itext t 1 "[関数] `fmap` [第1引数] `laf` [第2引数]"
	]

maybeList :: Page
maybeList = [ \t -> do
	writeTopTitle t "Maybeとリスト"
	text t "", \t -> do
	text t "* それぞれのアプリカティブ関数を比較する", \t -> do
	itext t 1 "maf :: Maybe (a -> b) -> Mayeb a -> Maybe b", \t -> do
	itext t 1 "laf :: [] (a -> b) -> [] a -> [] b", \t -> do
	text t "* 共通の枠組みは以下のようになる", \t -> do
	itext t 1 "f (a -> b) -> f a -> f b", \t -> do
	text t "* Control.Applicativeモジュールに以下の関数がある", \t -> do
	itext t 1 "(<*>) :: f (a -> b) -> f a -> f b"
	]

pure1 :: Page
pure1 = [ \t -> do
	writeTopTitle t "多引数関数"
	text t "", \t -> do
	text t "* 演算子(<$>)は以下のような型だ", \t -> do
	itext t 1 "(<*>) :: f (a -> b) -> f a -> f b", \t -> do
	text t "* 型変数bに(c -> d)を代入すると", \t -> do
	itext t 1 "f (a -> c -> d) -> f a -> f (c -> d)", \t -> do
	text t "* コンテナのなかの2引数関数に値をあたえて", \t -> do
	itext t 1 "コンテナのなかの1引数関数にすることができる", \t -> do
	text t "* さらにコンテナのなかの1引数関数に値をあたえられる", \t -> do
	text t "* つまり3引数, 4引数...関数にも使える", \t -> do
	itext t 1 "f <$> x <*> y <*> z <*> ..."
	]

pure2 :: Page
pure2 = [ \t -> do
	writeTopTitle t "pure"
	text t "", \t -> do
	text t "* 途中にコンテナにはいっていない値があるとする", \t -> do
	itext t 1 "f <$> x <*> y <*> z <*> ...", \t -> do
	text t "* yがコンテナにはいっていない値だとするとこれは動かない", \t -> do
	text t "* ここにyをいれるためにはyをコンテナにいれる必要がある", \t -> do
	text t "* コンテナにいれるための関数pureがある", \t -> do
	itext t 1 "pure :: a -> f a", \t -> do
	text t "* 関数pureを使えばよい", \t -> do
	itext t 1 "f <$> x <*> pure y <*> z <*> ...", \t -> do
	text t "* Maybe型やリストでのpureの定義は以下のようになる", \t -> do
	itext t 1 "pure = Just", \t -> do
	itext t 1 "pure x = [x]"
	]

applicative :: Page
applicative = [ \t -> do
	writeTopTitle t "Applicative"
	text t "", \t -> do
	text t "* 型クラスApplicativeは以下のようになる", \t -> do
	itext t 1 "class Applicative f where", \t -> do
	itext t 2 "pure :: a -> f a", \t -> do
	itext t 2 "(<*>) :: f (a -> b) -> f a -> f b"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* ファンクターの機能を多引数関数に拡張した"
	]
