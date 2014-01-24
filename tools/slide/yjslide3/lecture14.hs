import Lecture

subtitle :: String
subtitle = "第14回 ファンクター"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	list
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 多相型に対する型クラスを作ることができる", \t -> do
	text t "* 型変数をひとつとる多相型はコンテナと考えることができる", \t -> do
	itext t 1 "- コンテナとは値を格納するもの", \t -> do
	itext t 1 "- Maybe型: 0または1個の値を格納する", \t -> do
	itext t 1 "- リスト: 複数の値を順番に格納する", \t -> do
	itext t 1 "- 木: 複数の値を木構造として格納する", \t -> do
	text t "* これらのコンテナの構造そのものの性質を表現できる", \t -> do
	text t "* Functorはそのような型クラスのひとつ", \t -> do
	text t "* 「中身に関数を適用できる」という性質を型クラスにした"
 ]

prelude'' :: Page
prelude'' = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* リストに対するmap関数について考えてみる", \t -> do
	itext t 1 "map :: (a -> b) -> [a] -> [b]", \t -> do
	text t "* リストに格納されている値すべてに対して関数を適用する", \t -> do
	text t "* リスト[a]は([] a)という意味である", \t -> do
	text t "* つまり、リストは型変数をひとつとる多相型である"
 ]

prelude' :: Page
prelude' = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Functorと呼ばれる型クラスがある", \t -> do
	text t "* 型変数をひとつとる多相型の性質を表す型クラスである", \t -> do
	text t "* 型変数をひとつとる多相型には以下のものがある", \t -> do
	itext t 1 "- リスト", \t -> do
	itext t 1 "- Maybe型", \t -> do
	text t "* これらは実際にFunctorクラスのインスタンスとなっている", \t -> do
	text t "* 型変数をひとつとる多相型は値を格納するものと考えられる", \t -> do
	itext t 1 "- リスト: 0以上の値を格納するもの", \t -> do
	itext t 1 "- Maybe型: 0または1個の値を格納するもの", \t -> do
	text t "* Functorは「中身の値に関数を適用可」という性質を表す"
 ]

definition :: Page
definition = [\t -> do
	writeTopTitle t "Functorクラスの定義"
	text t "", \t -> do
	text t "* Functorクラスの定義は以下のようになっている", \t -> do
	itext t 1 "class Functor f where", \t -> do
	itext t 2 "fmap :: (a -> b) -> f a -> f b"
 ]

list :: Page
list = [\t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* リストの中身の値に関数を適用する関数は", \t -> do
	itext t 1 "map :: (a -> b) -> [a] -> [b]"
 ]
