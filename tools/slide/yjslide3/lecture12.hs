import Lecture

subtitle :: String
subtitle = "第12回 多相型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	typeVariable, typeVariable2, twice, twice2
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
	text t "* たとえば同じ型を2個集めた型を作るとする", \t -> do
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

twice3 :: Page
twice3 = [\t -> do
	writeTopTitle t "型変数"
 ]
