import Lecture

subtitle :: String
subtitle = "6. 型クラス"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, eq1, eq2, eq3, eq4, summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型クラスというものがある", \t -> do
	text t "* いくつかの型に共通する性質をまとめたものだ", \t -> do
	text t "* そのような性質は「特定の関数を持つ」ことで表現される"
	]

eq1 :: Page
eq1 = [ \t -> do
	writeTopTitle t "Eq"
	text t "", \t -> do
	text t "* 値が同じかどうかを判定できる性質を表すクラスEqがある", \t -> do
	text t "* クラスEqは以下のような定義となっている", \t -> do
	itext t 1 "class Eq a where", \t -> do
	itext t 2 "(==) :: a -> a -> Bool", \t -> do
	text t "* 型XがクラスEqで示される性質を持つことを", \t -> do
	itext t 1 "「型XはクラスEqのインスタンスである」と表現する"
	]

eq2 :: Page
eq2 = [ \t -> do
	writeTopTitle t "Eq"
	text t "", \t -> do
	text t "* 型Boolのインスタンス宣言は以下のようになる", \t -> do
	itext t 1 "instance Eq Bool where", \t -> do
	itext t 2 "True == True = True", \t -> do
	itext t 2 "False == False = True", \t -> do
	itext t 2 "_ == _ = False", \t -> do
	text t "* TrueとTrueは等しく、FalseとFalseは等しい", \t -> do
	text t "* それ以外の組み合わせは等しくない"
	]

eq3 :: Page
eq3 = [ \t -> do
	writeTopTitle t "Eq"
	text t "", \t -> do
	text t "* 型Maybeのインスタンス宣言は以下のようになる", \t -> do
	itext t 1 "instance Eq a => Eq (Maybe a) where", \t -> do
	itext t 2 "Maybe x == Maybe y = x == y", \t -> do
	itext t 2 "Nothing == Nothing = True", \t -> do
	itext t 2 "_ == _ = False", \t -> do
	text t "* 'Eq a =>'の部分を型制約と呼ぶ", \t -> do
	text t "* 型変数aにはEqクラスのインスタンスである型のみが入る", \t -> do
	text t "* この制約は'x == y'をするために必要となる"
	]

eq4 :: Page
eq4 = [ \t -> do
	writeTopTitle t "Eq"
	text t "", \t -> do
	text t "* [a]のインスタンス宣言は以下のようになる", \t -> do
	itext t 1 "instance Eq a => Eq [a] where", \t -> do
	itext t 2 "(x : xs) == (y : ys) = x == y && xs == ys", \t -> do
	itext t 2 "[] == [] = True", \t -> do
	itext t 2 "_ == _ = False", \t -> do
	text t "* 2つのリストが等しいためには先頭同士が等しく", \t -> do
	itext t 1 "残りのリスト同士も等しい必要がある", \t -> do
	text t "* 空リスト同士は等しく", \t -> do
	itext t 1 "空リストと空でないリストとは等しくない"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 型クラスEqを例に型クラス宣言とインスタンス宣言とを見た"
	]
