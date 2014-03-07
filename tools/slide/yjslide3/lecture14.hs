module Main (main) where

import Data.Char

import Lecture

subtitle :: String
subtitle = "第14回 ファンクター"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	list, appMaybe1, mapTree1, mapTree2, compareMaps,
	functor, treeToFunctor, whatsGood, whatsGood2, summary
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

list :: Page
list = [\t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* リストの中身の値に関数を適用する関数は", \t -> do
	itext t 1 "map :: (a -> b) -> [a] -> [b]", \t -> do
	text t "* 試してみる", \t -> do
	text t "* lectures/lecture14ディレクトリを作ってそこに移動", \t -> do
	itext t 1 "Prelude> map even [1 .. 5]", \t -> do
	itext t 1 $ show $ map even [1 :: Int .. 5]
 ]

appMaybe :: (a -> b) -> Maybe a -> Maybe b
appMaybe _ Nothing = Nothing
appMaybe f (Just x) = Just $ f x

appMaybe1 :: Page
appMaybe1 = [\t -> do
	writeTopTitle t "Maybe"
	text t "", \t -> do
	text t "* Maybe型の中身の値に関数を適用する関数を書いてみる", \t -> do
	itext t 1 "appMaybe :: (a -> b) -> Maybe a -> Maybe b", \t -> do
	itext t 1 "appMaybe _ Nothing = Nothing", \t -> do
	itext t 1 "appMaybe f (Just x) = Just $ f x", \t -> do
	text t "* lecture/lecture14/functor.hsに書き込もう", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "Prelude> :load functor.hs", \t -> do
	itext t 1 "*Main> appMaybe even Nothing", \t -> do
	itext t 1 $ show $ appMaybe (even :: Int -> Bool) Nothing, \t -> do
	itext t 1 "*Main> appMaybe even (Just 4)", \t -> do
	itext t 1 $ show $ appMaybe even (Just 4 :: Maybe Int)
 ]

mapTree1 :: Page
mapTree1 = [\t -> do
	writeTopTitle t "木"
	text t "", \t -> do
	text t "* 二分木を作ろう", \t -> do
	itext t 1 "data BinTree a"
	itext t 2 "= Node (BinTree a) (BinTree a)"
	itext t 2 "| Leaf a"
	itext t 2 "deriving Show", \t -> do
	text t "* 二分木のすべての葉に関数を適用する関数", \t -> do
	itext t 1 "mapTree :: (a -> b) -> BinTree a -> BinTree b", \t -> do
	itext t 1 "mapTree f (Leaf x) = Leaf $ f x", \t -> do
	itext t 1 "mapTree f (Node tl tr) ="
	itext t 2 "Node (mapTree f tl) (mapTree f tr)", \t -> do
	text t "* これをfunctor.hsに書き込もう"
 ]

data BinTree' a = Node (BinTree' a) (BinTree' a) | Leaf a deriving Show

mapTree :: (a -> b) -> BinTree' a -> BinTree' b
mapTree f (Leaf x) = Leaf $ f x
mapTree f (Node tl tr) = Node (mapTree f tl) (mapTree f tr)

tree :: BinTree' Int
tree = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))

mapTree2 :: Page
mapTree2 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "*Main> :reload", \t -> do
	text t "*Main> let t = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))", \t -> do
	text t "*Main> mapTree even t", \t -> do
	itext t 1 $ show $ mapTree even tree, \t -> do
	text t "*Main> mapTree (* 3) t", \t -> do
	itext t 1 $ show $ mapTree (* 3) tree
 ]

compareMaps :: Page
compareMaps = [\t -> do
	writeTopTitle t "それぞれのmap"
	text t "", \t -> do
	text t "* それぞれの型に対するmap的関数", \t -> do
	itext t 1 "map :: (a -> b) -> [] a -> [] b", \t -> do
	itext t 1 "appMaybe :: (a -> b) -> Maybe a -> Maybe b", \t -> do
	itext t 1 "mapTree :: (a -> b) -> BinTree a -> BinTree b", \t -> do
	text t "* これらの型を比較してみると以下の形が得られる", \t -> do
	itext t 1 "(a -> b) -> f a -> f b", \t -> do
	text t "* fにそれぞれの型を入れれば良い", \t -> do
	text t "* 型fの「中身に関数を適用可」という性質を言い換えると", \t -> do
	itext t 1 "- 「(a -> b) -> f a -> f b型の関数を持つ」となる"
 ]

functor :: Page
functor = [\t -> do
	writeTopTitle t "Functor"
	text t "", \t -> do
	text t "* 「中身に関数を適用可」を表すFunctorクラスがある", \t -> do
	itext t 1 "class Functor f where", \t -> do
	itext t 2 "fmap :: (a -> b) -> f a -> f b", \t -> do
	text t "* リストやMaybeはFunctorクラスのインスタンス", \t -> do
	itext t 1 "- リストに対するfmapはmapと同じ", \t -> do
	itext t 1 "- Maybeに対するfmapはappMaybeと同じ", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> fmap even [1 .. 5]", \t -> do
	itext t 1 $ show $ fmap even [1 :: Int .. 5], \t -> do
	itext t 1 "*Main> fmap even $ Just 8", \t -> do
	itext t 1 $ show $ fmap even $ Just (8 :: Int)
 ]

instance Functor BinTree' where
	fmap = mapTree

treeToFunctor :: Page
treeToFunctor = [\t -> do
	writeTopTitle t "木をFunctorにする"
	text t "", \t -> do
	text t "* BinTreeは自作のデータ型なのでまだFunctorじゃない", \t -> do
	text t "* Functorにする", \t -> do
	itext t 1 "instance Functor BinTree where", \t -> do
	itext t 2 "fmap = mapTree", \t -> do
	text t "* これをfunctor.hsに書き込もう", \t -> do
	text t "* 試してみよう", \t -> do
	text t "*Main> :reload"
	text t "*Main> let t = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))", \t -> do
	text t "*Main> fmap even t", \t -> do
	text t $ show $ fmap even tree
 ]

toCode :: Functor f => f Char -> f Int
toCode = fmap ord

whatsGood :: Page
whatsGood = [\t -> do
	writeTopTitle t "何がうれしいの?"
	text t "", \t -> do
	text t "* 値が何らかの構造のなかに存在する場合に", \t -> do
	text t "* その構造とは関係なく値に何かをしたい場合がある", \t -> do
	text t "* 例えば文字が入っている「何か」に対して", \t -> do
	text t "* その文字を「文字コード」に変えたいとする", \t -> do
	text t "* そのとき、その「何か」がFunctorであれば以下が使える", \t -> do
	itext t 1 "toCode :: Functor f => f Char -> f Int", \t -> do
	itext t 1 "toCode = fmap ord", \t -> do
	text t "* これをfunctor.hsに書き込もう", \t -> do
	text t "* Data.Charモジュールのordを使うので以下を先頭に書こう", \t -> do
	itext t 1 "import Data.Char (ord)"
 ]

whatsGood2 :: Page
whatsGood2 = [\t -> do
	writeTopTitle t "試してみる"
	itext t (- 1) "", \t -> do
	itext t (- 1)"*Main> :reload", \t -> do
	itext t (- 1) "*Main> toCode ['a', 'b', 'c']", \t -> do
	itext t (- 1) $ show $ toCode ['a', 'b', 'c'], \t -> do
	itext t (- 1) "*Main> toCode $ Just 'd'", \t -> do
	itext t (- 1) $ show $ toCode $ Just 'd', \t -> do
	itext t (- 1) "*Main> let t = Node (Leaf 'e') (Node (Leaf 'f') (Leaf 'g'))", \t -> do
	itext t (- 1) "*Main> toCode t", \t -> do
	itext t (- 1) $ show $ toCode $ Node (Leaf 'e') (Node (Leaf 'f') (Leaf 'g')), \t -> do
	text t "* 様々なデータ構造においてその中身に同じ変換が行えた"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 多相型に対する型クラスの例としてFunctorを取り上げた", \t -> do
	text t "* Functorは「中身に対して関数適用可」を表す型クラス", \t -> do
	text t "* 以下のクラス関数を持っている", \t -> do
	itext t 1 "fmap :: (a -> b) -> f a -> f b", \t -> do
	text t "* これはfという構造中の型aの値に関数を適用する関数", \t -> do
	text t "* Functorによって抽象化された構造を使えば", \t -> do
	itext t 1 "- コンテナの種類にかかわらず", \t -> do
	itext t 1 "- 値に対する操作を定義することができる"
 ]
