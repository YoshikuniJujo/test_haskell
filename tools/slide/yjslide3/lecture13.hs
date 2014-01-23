import Lecture
import Automaton

subtitle :: String
subtitle = "第13回 型クラス"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, classPrelude,
	checkClass, checkClass2,
	toOrd, toEq, toEq2, toOrd2, toOrd3, toOrd4,
	toInstanceSummary,
	aboutDeriving,
	automaton, automaton2, automaton3, automaton4, automaton5,
	automaton6, automaton7, automaton8, automaton9, automaton10
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

automaton :: Page
automaton = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* オートマトンというものがある", \t -> do
	text t "* 入力によって次々に状態を変化させていくもの", \t -> do
	text t "* 初期状態と受理状態とがある", \t -> do
	text t "* 初期状態に対して入力を次々と与えていき", \t -> do
	itext t 1 "- 入力が終わったときに状態が受理状態であれば", \t -> do
	itext t 1 "- その入力列を受理するという", \t -> do
	text t "* 単純な機械をモデル化したものと考えられる", \t -> do
	text t "* これを型クラスという仕組みを活用して作ってみよう"
 ]

automaton2 :: Page
automaton2 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* 状態を丸で表し", \t -> do
	text t "* 状態間の遷移を矢印で表す", \t -> do
	itext t 1 "- 矢印のそばにその遷移を引き起こす入力を書く", \t -> do
	text t "* 初期状態には矢印を追加する", \t -> do
	text t "* 受理状態は二重丸とする", \t -> do
	text t "* 例:"
	m1 t 100 240, \t -> do
	text t "* 入力列が0, 1, 1, 0, 0の場合", \t -> do
	itext t 1 "q1 -> q1 -> q2 -> q2 -> q3 -> q2", \t -> do
	text t "* q2は受理状態なのでこの入力列は受理される"
 ]

m1 :: Turtle -> Double -> Double -> IO ()
m1 t x y = do
	initialQ t x y "q1"
	selfQ t "0"
	nextQ t "1" "q2"
	selfQ t "1"
	acceptQ t
	nextQ t "0" "q3"
	backQ t "0, 1"
	rtGoto t x (y + 40)

automaton3 :: Page
automaton3 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* 今のオートマトンを再掲する", \t -> do
	m1 t 100 140, \t -> do
	text t "* このオートマトンが受理するのは以下の入力列となる", \t -> do
	itext t 1 "- すくなくともひとつの1を含み", \t -> do
	itext t 1 "- 最後の1のあとには偶数個(0も可)の0が来る", \t -> do
	text t "* いろいろな例で確認してみよう", \t -> do
	itext t 1 "(1分)"
 ]

automaton4 :: Page
automaton4 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* ある型がオートマトンの状態であることを表す"
	itext t 1 "AMStateクラスを作っていく", \t -> do
	text t "* 入力値は0, 1値とし、それを以下で表現する", \t -> do
	itext t 1 "data OI = O | I deriving Show", \t -> do
	text t "* これをautomaton.hsに書き込もう", \t -> do
	text t "* オートマトンの状態であるために必要なのは何かを考える", \t -> do
	itext t 1 "1. ある状態で入力によって次にどの状態に移るか", \t -> do
	itext t 1 "2. 初期状態", \t -> do
	itext t 1 "3. 状態が受理状態であるかどうかのチェック"
 ]

automaton5 :: Page
automaton5 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* 状態を表す型を型変数qで表すことにする", \t -> do
	text t "* 「1. 状態の遷移」を表す関数stepが必要", \t -> do
	itext t 1 "- これは現在の状態と入力値を引数とし", \t -> do
	itext t 1 "- 遷移先の状態を返す関数なので", \t -> do
	arrowIText t 1 "step :: q -> OI -> q", \t -> do
	text t "* 「2. 初期状態」は「状態」なので", \t -> do
	arrowIText t 1 "start :: q", \t -> do
	text t "* 「3. 状態が受理状態であるかどうかのチェック」は", \t -> do
	itext t 1 "- 状態をとりBool値を返せば良いので", \t -> do
	arrowIText t 1 "accept :: q -> Bool"
 ]

automaton6 :: Page
automaton6 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* よってオートマトンの状態であることを示すクラスは", \t -> do
	itext t 1 "class AMState q where", \t -> do
	itext t 2 "step :: q -> OI -> q", \t -> do
	itext t 2 "start :: q", \t -> do
	itext t 2 "accept :: q -> Bool", \t -> do
	text t "* これはどんな型であっても", \t -> do
	itext t 1 "- step, start, acceptを定義しさえすれば", \t -> do
	itext t 1 "- オートマトンの状態となるということ"
 ]

automaton7 :: Page
automaton7 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* AMStateのインスタンスに対する関数を作ることができる", \t -> do
	text t "* 入力列に対する状態遷移の結果を返す関数", \t -> do
	itext t 1 "run :: AMState q => q -> [OI] -> q", \t -> do
	itext t 1 "run q [] = q", \t -> do
	itext t 1 "run q (oi : ois) = run (step q oi) ois", \t -> do
	text t "* 入力がなければ状態はそのまま", \t -> do
	text t "* 入力があれば"
	itext t 1 "その入力に対して状態を遷移させたうえで入力を続ける", \t -> do
	text t "* automaton.hsに書き込もう"
 ]

automaton8 :: Page
automaton8 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* ある入力列が受理されるかどうかを判定する関数は", \t -> do
	itext t 1 "isAccept :: AMState q => [OI] -> Bool", \t -> do
	itext t 1 "isAccept = accept . run start", \t -> do
	text t "* で良さそうだが、これは動かない", \t -> do
	text t "* isAcceptの引数は[OI]で返り値はBoolである", \t -> do
	arrowIText t 1 "どこにもqが出てこない", \t -> do
	arrowIText t 1 "startの型が決められない", \t -> do
	text t "* ダミーの引数を使うことになる", \t -> do
	itext t 1 "isAccept :: AMState q => q -> [OI] -> Bool", \t -> do
	itext t 1 "isAccept q = accept . run (start `asTypeOf` q)", \t -> do
	text t "* automaton.hsに書き込もう"
 ]

data OI = O | I deriving Show

class AMState q where
	step :: q -> OI -> q
	start :: q
	accept :: q -> Bool

run :: AMState q => q -> [OI] -> q
run = foldl step

isAccept :: AMState q => q -> [OI] -> Bool
isAccept q = accept . run (start `asTypeOf` q)

data Q1 = Q11 | Q12 | Q13 deriving Show

-- Eq, Ord, Enum, Ix, Bounded, Show, Read

automaton9 :: Page
automaton9 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* それでは最初の例を表すオートマトンを作ってみよう", \t -> do
	m1 t 100 140, \t -> do
	text t "* 型をQ1としてその型の値をQ11, Q12, Q13とする", \t -> do
	itext t 1 "data Q1 = Q11 | Q12 | Q13 deriving Show"
 ]

automaton10 :: Page
automaton10 = [\t -> do
	writeTopTitle t "オートマトン"
	text t "", \t -> do
	text t "* AMStateのインスタンスにしてみよう", \t -> do
	hideturtle t
	m1 t 100 140
	showturtle t, \t -> do
	text t "* まずはstepQ1を定義しよう", \t -> do
	itext t 1 "stepQ1 Q11 O = Q11", \t -> do
	itext t 1 "stepQ1 Q11 I = Q12", \t -> do
	itext t 1 "stepQ1 Q12 O = Q13", \t -> do
	itext t 1 "stepQ1 Q12 I = Q12", \t -> do
	itext t 1 "stepQ1 Q13 _ = Q12"
 ]
