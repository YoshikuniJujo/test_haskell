import Lecture

subtitle :: String
subtitle = "第13回 型クラス"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, classPrelude,
	checkClass, checkClass2,
	toOrd, toEq, toEq2, toOrd2, toOrd3, toOrd4,
	toInstanceSummary,
	aboutDeriving
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 複数の型に共通の性質が存在する", \t -> do
	itext t 1 "- 大小比較ができること", \t -> do
	itext t 1 "- 文字列として表示できること", \t -> do
	itext t 1 "- 等々", \t -> do
	text t "* ある型がその「性質」を持つということは", \t -> do
	itext t 1 "- それを表現する関数が型に対して定義されていること", \t -> do
	text t "* つまり(>)や(<)がIntに対して定義されているので", \t -> do
	itext t 1 "- Int型は「大小比較可」という性質を持つ"
 ]

classPrelude :: Page
classPrelude = [\t -> do
	writeTopTitle t "型クラス"
	text t "", \t -> do
	text t "* Haskellでは、そのような性質を型クラスで表現する", \t -> do
	text t "* 型TがクラスCの性質を持つということを", \t -> do
	itext t 1 "- 「型TはクラスCのインスタンスである」と表現する", \t -> do
	text t "* いくつかの例を挙げる", \t -> do
	text t "* Ordは「大小比較可」という性質を表すクラスであり", \t -> do
	text t "* Showは「表示可」という性質を表すクラスである", \t -> do
	itext t 1 "- Int型はOrdクラスのインスタンスである", \t -> do
	itext t 1 "- Char型はShowクラスのインスタンスである", \t -> do
	itext t 1 "- 等々"
 ]

checkClass :: Page
checkClass = [\t -> do
	writeTopTitle t "型クラスのチェック"
	text t "", \t -> do
	text t "* コマンドプロンプトを2つ立ち上げて", \t -> do
	text t "* lectures/lecture13を作りそこに移動しよう", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> :info Char", \t -> do
	itext t 1 "...", \t -> do
	itext t 1 "instance Ord Char ...", \t -> do
	itext t 1 "...", \t -> do
	itext t 1 "instance Show Char ...", \t -> do
	text t "* その型がどのクラスのインスタンスであるかを調べるには", \t -> do
	itext t 1 "- ghciで:info [型名]とする", \t -> do
	text t "* CharがOrdとShowのインスタンスであることがわかった"
 ]

checkClass2 :: Page
checkClass2 = [\t -> do
	writeTopTitle t "クラス関数のチェック"
	text t "", \t -> do
	text t "* ある型クラスにどんなクラス関数があるかを知るには", \t -> do
	itext t 1 "Prelude> :info Ord", \t -> do
	itext t 1 "class Eq a => Ord a where"
	itext t 2 "compare :: a -> a -> Ordering"
	itext t 2 "(<) :: a -> a -> Bool"
	itext t 2 "...", \t -> do
	text t "* Ordクラスにはcompare, (<)などがある", \t -> do
	text t "* また'Eq a =>'とあるのは以下を意味する", \t -> do
	text t "* Ordクラスのインスタンスであるためには", \t -> do
	itext t 1 "Eqクラスのインスタンスである必要がある"
 ]

data Size = Short | Tall | Grande | Venti deriving Show

instance Eq Size where
	Short == Short = True
	Tall == Tall = True
	Grande == Grande = True
	Venti == Venti = True
	_ == _ = False

instance Ord Size where
	Short <= _ = True
	_ <= Short = False
	Tall <= _ = True
	_ <= Tall = False
	Grande <= _ = True
	_ <= Grande = False
	Venti <= _ = True

toOrd :: Page
toOrd = [\t -> do
	writeTopTitle t "自作の型をOrdとする"
	text t "", \t -> do
	text t "* 型クラスは自分で作れるが", \t -> do
	text t "* まずは自作の型を既存の型クラスのインスタンスにしよう", \t -> do
	text t "* スターバックスのカップのサイズを表す型を作る", \t -> do
	itext t 1 "data Size = Short | Tall | Grande | Venti", \t -> do
	text t "* これをclass.hsに保存しよう", \t -> do
	text t "* これは大小比較可なのでOrdクラスのインスタンスにしよう", \t -> do
	text t "* さっき見たようにOrdクラスのインスタンスにするためには", \t -> do
	itext t 1 "- Eqクラスのインスタンスにする必要がある", \t -> do
	text t "* Eqクラスは同値かどうか判定可という性質を表現するクラス"
 ]

toEq :: Page
toEq = [\t -> do
	writeTopTitle t "自作の型をEqとする"
	text t "", \t -> do
	text t "* Eqクラスのクラス関数には(==), (/=)がある", \t -> do
	text t "* このクラスのインスタンスにするには(==)を定義する", \t -> do
	text t "* (==)を定義しておけば(/=)にはデフォルトの定義が使われる", \t -> do
	text t "* 実行効率あるいはその他の理由で(/=)を別に定義することも", \t -> do
	text t "* SizeをEqクラスのインスタンスにしてみよう", \t -> do
	itext t 1 "instance Eq Size where", \t -> do
	itext t 2 "Short == Short = True", \t -> do
	itext t 2 "Tall == Tall = True", \t -> do
	itext t 2 "Grande == Grande = True", \t -> do
	itext t 2 "Venti == Venti = True", \t -> do
	itext t 2 "_ == _ = False", \t -> do
	text t "* これをclass.hsに書き込もう"
 ]

toEq2 :: Page
toEq2 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "Prelude> :load class.hs", \t -> do
	itext t 1 "*Main> Short == Short", \t -> do
	itext t 1 $ show $ Short == Short, \t -> do
	itext t 1 "*Main> Tall == Venti", \t -> do
	itext t 1 $ show $ Tall == Venti, \t -> do
	itext t 1 "*Main> Grande /= Grande", \t -> do
	itext t 1 $ show $ Grande /= Grande, \t -> do
	itext t 1 "*Main> Venti /= Short", \t -> do
	itext t 1 $ show $ Venti /= Short
 ]

toOrd2 :: Page
toOrd2 = [\t -> do
	writeTopTitle t "自作の型をOrdにする"
	text t "", \t -> do
	text t "* Ordクラスには7つのクラス関数がある", \t -> do
	itext t 1 "compare, (<), (>=), (>), (<=), max, min", \t -> do
	text t "* すべて定義しても良いが", \t -> do
	itext t 1 "- compareまたは(<=)のどちらかを定義すれば", \t -> do
	itext t 1 "- 他の関数はデフォルトの値が使われる"
 ]

toOrd3 :: Page
toOrd3 = [\t -> do
	writeTopTitle t "自作の型をOrdにする"
	text t "", \t -> do
	text t "* SizeをOrdのインスタンスにする", \t -> do
	itext t 1 "instance Ord Size where"
	itext t 2 "Short <= _ = True"
	itext t 2 "_ <= Short = False"
	itext t 2 "Tall <= _ = True"
	itext t 2 "_ <= Tall = False"
	itext t 2 "Grande <= _ = True"
	itext t 2 "_ <= Grande = False"
	itext t 2 "Venti <= _ = True", \t -> do
	text t "* これをclass.hsに書き込もう"
 ]

toOrd4 :: Page
toOrd4 = [\t -> do
	writeTopTitle t "試してみる"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> Short < Short", \t -> do
	itext t 1 $ show $ Short < Short, \t -> do
	itext t 1 "*Main> Grande <= Grande", \t -> do
	itext t 1 $ show $ Grande <= Grande, \t -> do
	itext t 1 "*Main> Tall >= Venti", \t -> do
	itext t 1 $ show $ Tall >= Venti, \t -> do
	itext t 1 "*Main> Venti > Short", \t -> do
	itext t 1 $ show $ Venti > Short
 ]

toInstanceSummary :: Page
toInstanceSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* 複数の型に共通した性質がある", \t -> do
	text t "* 性質はその型を扱う関数によって表される", \t -> do
	text t "* そのような性質、つまり関数をまとめたものが型クラス", \t -> do
	text t "* 型クラスのインスタンスにするには関数の中身を定義する", \t -> do
	text t "* 構文は以下のようになる", \t -> do
	itext t 1 "instance [型クラス] [型] where", \t -> do
	itext t 2 "[関数定義1]", \t -> do
	itext t 2 "[関数定義2]", \t -> do
	itext t 2 "..."
 ]

aboutDeriving :: Page
aboutDeriving = [\t -> do
	writeTopTitle t "deriving"
	text t "", \t -> do
	text t "* 実はさっきやったことは以下の定義で実現できる", \t -> do
	itext t 1 "data Size = Short | Tall | Grande | Venti"
	itext t 2 "deriving (Eq, Ord)", \t -> do
	text t "* 使用頻度の高い以下のクラスについては"
	itext t 1 "derivingで簡単にインスタンスを導出できる", \t -> do
	itext t 1 "Eq, Ord, Enum, Ix, Bounded, Show, Read", \t -> do
	text t "* derivingを使う場合は値構築子を"
	itext t 1 "「小さいものから大きいものへ」の順に並べる"
 ]

-- Eq, Ord, Enum, Ix, Bounded, Show, Read
