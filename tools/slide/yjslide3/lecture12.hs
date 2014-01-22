import Lecture

subtitle :: String
subtitle = "第12回 多相型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	typeVariable, typeVariable2, twice, twice2,
	tuple, tuple2, tuple3, tuple4,
	list, list2, list3
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回は代数的データ型について見た", \t -> do
	text t "* 以下のような構文を学んだ", \t -> do
	itext t 1 "data [型構築子]"
	itext t 2 "= [値構築子1] [型11] [型12] ..."
	itext t 2 "| [値構築子2] [型21] [型22] ..."
	itext t 2 "...", \t -> do
	text t "* 今回はこれを拡張し", \t -> do
	itext t 1 "- より柔軟性のある定義をする", \t -> do
	itext t 1 "- 「構造」を抽象化した型を作る"
 ]

typeVariable :: Page
typeVariable = [\t -> do
	writeTopTitle t "型変数"
	text t "", \t -> do
	text t "* 普通の変数と同じように同じ型変数aには同じ型が入る", \t -> do
	text t "* つまりid :: a -> aのように宣言された場合", \t -> do
	itext t 1 "- 以下はidの型として正当である", \t -> do
	itext t 2 "Int -> Int"
	itext t 2 "Char -> Char"
	itext t 2"(Int -> Int) -> (Int -> Int)"
	itext t 2 "[Char] -> [Char]", \t -> do
	itext t 1 "- 以下はidの型にはならない", \t -> do
	itext t 2 "Int -> Char"
	itext t 2 "(Int -> Int) -> Int"
	itext t 2 "[Char] -> Char"
 ]

typeVariable2 :: Page
typeVariable2 = [\t -> do
	writeTopTitle t "型変数"
	text t "", \t -> do
	text t "* 型変数は複数の型を同時に定義するのに使える", \t -> do
	text t "* 今見たようにid :: a -> aという定義は", \t -> do
	itext t 1 "Int -> Int, Char -> Char ...を同時に定義している"
 ]

twice :: Page
twice = [\t -> do
	writeTopTitle t "多相型"
	text t "", \t -> do
	text t "* たとえば同じ型の値を2個集めた値の型を作るとする", \t -> do
	text t "* このときも型変数が使える", \t -> do
	itext t 1 "data Twice a = Twice a a", \t -> do
	text t "* 以下のすべてを定義したことになる", \t -> do
	itext t 1 "data Twice Int = Twice Int Int", \t -> do
	itext t 1 "data Twice Char = Twice Char Char", \t -> do
	itext t 1 "data Twice [Int] = Twice [Int] [Int]", \t -> do
	itext t 1 "data Twice (Int -> Bool) ="
	itext t 2 "Twice (Int -> Bool) (Int -> Bool)", \t -> do
	itext t 2 "...", \t -> do
	text t "* このように様々な型の総称となるような型を多相型と呼ぶ"
 ]

data Twice a = Twice a a deriving Show

mapTwice :: (a -> b) -> Twice a -> Twice b
mapTwice f (Twice x y) = Twice (f x) (f y)

twice2 :: Page
twice2 = [\t -> do
	writeTopTitle t "多相型"
	text t "", \t -> do
	text t "* この2つの値に対して関数を適用する関数を書こう", \t -> do
	itext t 1 "mapTwice :: (a -> b) -> Twice a -> Twice b", \t -> do
	itext t 1 "mapTwice f (Twice x y) = Twice (f x) (f y)", \t -> do
	text t "* 型の定義をghciでの表示に対応するように変えて", \t -> do
	itext t 1 "data Twice a = Twice a a deriving Show", \t -> do
	text t "* lectures/lecture12ディレクトリのdata.hsに書き込もう", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> mapTwice even (Twice 3 8)", \t -> do
	itext t 1 $ show $ mapTwice even $ Twice (3 :: Int) 8
 ]

data Tuple a b = Tuple a b deriving Show

tuple :: Page
tuple = [\t -> do
	writeTopTitle t "タプル"
	text t "", \t -> do
	text t "* タプルと同等のものを自分で定義することができる", \t -> do
	itext t 1 "data Tuple a b = Tuple a b deriving Show", \t -> do
	text t "* これをdata.hsに書き込もう", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> Tuple 8 'c'", \t -> do
	itext t 1 $ show $ Tuple (8 :: Int) 'c'
 ]

myFst :: Tuple a b -> a
myFst (Tuple x _) = x

mySnd :: Tuple a b -> b
mySnd (Tuple _ y) = y

tuple2 :: Page
tuple2 = [\t -> do
	writeTopTitle t "タプル"
	text t "", \t -> do
	text t "* 自作のタプルの要素を取り出す関数を書いてみる", \t -> do
	itext t 1 "myFst :: Tuple a b -> a", \t -> do
	itext t 1 "myFst (Tuple x _) = x", \t -> do
	itext t 1 "mySnd :: Tuple a b -> b", \t -> do
	itext t 1 "mySnd (Tuple _ y) = y", \t -> do
	text t "* data.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> myFst $ Tuple 3 'c'", \t -> do
	itext t 1 $ show $ myFst $ Tuple (3 :: Int) 'c', \t -> do
	itext t 1 "*Main> mySnd $ Tuple 3 'c'", \t -> do
	itext t 1 $ show $ mySnd $ Tuple (3 :: Int) 'c'
 ]

tuple3 :: Page
tuple3 = [\t -> do
	writeTopTitle t "タプル"
	text t "", \t -> do
	text t "* 型についても値についても類似した構文糖がある", \t -> do
	itext t 1 "- 型については(a, b)は脱糖すると(,) a bとなり", \t -> do
	itext t 1 "- 値についても(x, y)は脱糖すると(,) x yとなる", \t -> do
	text t "* つまり脱糖すると以下のような定義となる", \t -> do
	itext t 1 "someTuple :: (,) Int Char", \t -> do
	itext t 1 "someTuple = (,) 3 'c'", \t -> do
	text t "* タプルは以下のように定義されていることになる", \t -> do
	itext t 1 "data (,) a b = (,) a b", \t -> do
	text t "* これは特殊な識別子(,)が使われていること以外は", \t -> do
	itext t 1 "Tuple型の定義と等しい"
 ]

tuple4 :: Page
tuple4 = [\t -> do
	writeTopTitle t "タプル"
	text t "", \t -> do
	text t "* タプルは特別な名前と構文糖以外については", \t -> do
	itext t 1 "代数的データ型の定義の範囲内で定義することができる"
 ]

-- infixr 6 `Cons`

data List a = Cons a (List a) | Nil deriving Show

list :: Page
list = [\t -> do
	writeTopTitle t "リスト"
	text t "", \t -> do
	text t "* リストに関しても同様に自分で定義することができる", \t -> do
	itext t 1 "data List a = Cons a (List a) | Nil deriving Show", \t -> do
	text t "* data.hsに書き込んで試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> 3 `Cons` (4 `Cons` (7 `Cons` Nil))", \t -> do
	itext t 1 $ show $ (3 :: Int) `Cons` (4 `Cons` (7 `Cons` Nil)), \t -> do
	text t "* Listに対するmap関数を書いてみよう", \t -> do
	itext t 1 "mapList :: (a -> b) -> List a -> List b", \t -> do
	itext t 1 "mapList _ Nil = Nil", \t -> do
	itext t 1 "mapList f (x `Cons` xs) = f x `Cons` mapList f xs", \t -> do
	text t "* これもdata.hsに書き込んでおこう"
 ]

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (x `Cons` xs) = f x `Cons` mapList f xs

list2 :: Page
list2 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 0 "*Main> :reload", \t -> do
	itext t 0 "*Main> mapList even $ 3 `Cons` (4 `Cons` (7 `Cons` Nil))", \t -> do
	itext t 0 $ show $ mapList even $ (3 :: Int) `Cons` (4 `Cons` (7 `Cons` Nil)), \t -> do
	text t "* リストについても構文糖があり", \t -> do
	itext t 1 "- 型については[a]が[] aとなり", \t -> do
	itext t 1 "- 値については[x, y, z]がx : y : z : []となる", \t -> do
	text t "* リストは以下のように定義されていると考えられる", \t -> do
	itext t 1 "data [] a = (:) a ([] a) | []", \t -> do
	text t "* Listは[]に、Consは(:)に、Nilは[]に対応している"
 ]

list3 :: Page
list3 = [\t -> do
	writeTopTitle t "再帰的な型"
	text t "", \t -> do
	text t "* リストの定義を再掲する", \t -> do
	itext t 1 "data [] a = (:) a ([] a) | []", \t -> do
	text t "* ([] a)の定義のなかに([] a)が出てくる", \t -> do
	text t "* つまり、再帰的な定義となっている", \t -> do
	text t "* 構文糖を適用するとわかりやすくなる", \t -> do
	itext t 1 "data [a] = a : [a] | []", \t -> do
	text t "* aのリストは空リストまたはaのリストにaの値を足したもの"
 ]
