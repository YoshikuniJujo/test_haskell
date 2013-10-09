module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第7回 型クラス"

pages :: [Page]
pages = [
	titlePage, prelude,
	simpleExam1, simpleExam2, eqShowSummary,
--	duckTyping, color, vegetable, fruit, fortune
--	naDatabase
	grow, buri, bora, growing, growInt, future,
	futureGrowing1, futureGrowing2, futureSummary,
	queuePrelude, queue1, queue2, queue3, queue4, queue5,
	queue6, queue7,
	defaultMethod,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型クラスはHaskellのなかで一番好きな機能", \t -> do
	itext t 1 "- 誰が?", \t -> do
	itext t 1 "- 僕が", \t -> do
	text t "* なんとなく気持ちがいい", \t -> do
	itext t 1 "- 何が?", \t -> do
	itext t 1 "- インターフェースをさきに定義"
	itext t 1 "- 実装はあと", \t -> do
	text t "* Haskellで多重定義(オーバーロード)を使うための枠組"
 ]

simpleExam1 :: Page
simpleExam1 = [\t -> do
	writeTopTitle t "簡単な例", \t -> do
	text t "type Name = String"
	text t "data Animal = Dog Name | Cat Name"
	text t "", \t -> do
	text t "Animalを(==)で比較したい"
	text t "", \t -> do
	text t "instance Eq Animal where"
	itext t 1 "Dog n1 == Dog n2 = n1 == n2"
	itext t 1 "Cat n1 == Cat n2 = n1 == n2"
	itext t 1 "_ == _ = False"
	text t "", \t -> do
	text t "Dog \"taro\" == Dog \"taro\" => True"
	text t "Dog \"taro\" == Dog \"hanako\" => False"
	text t "Dog \"taro\" == Cat \"taro\" => False"
 ]

simpleExam2 :: Page
simpleExam2 = [\t -> do
	writeTopTitle t "簡単な例"
	text t "Animalを表示したい"
	text t "", \t -> do
	text t "instance Show Animal where"
	itext t 1 "show (Dog n) = \"Dog \" ++ show n"
	itext t 1 "show (Cat n) = \"Cat \" ++ show n"
	text t "", \t -> do
	text t "show (Dog \"taro\") => \"Dog \\\"taro\\\"\""
	text t "show (Cat \"hanako\") => \"Cat \\\"hanako\\\"\""
 ]

eqShowSummary :: Page
eqShowSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* EqやShowははじめから定義されている型クラス", \t -> do
	text t "* 型クラスにはderiving節で簡単に定義できるものも", \t -> do
	itext t 1 "Eq, Ord, Enum, Ix, Bounded, Read, Show"
	itext t 1 "これらはたとえば"
	itext t 1 "data ... = ... deriving (Eq, Show)"
	itext t 1 "とするだけで「よきにはから」ってくれる", \t -> do
	text t "* 型クラスは定義ずみのものだけではない"
	text t "", \t -> do
	semititle t "自分で定義することができる"
 ]

duckTyping :: Page
duckTyping = [\t -> do
	writeTopTitle t "あひる"
	text t "", \t -> do
	text t "「あひるのように歩きあひるのように鳴くのなら、"
	itext t 1 "それはあひるである」"
	text t "", \t -> do
	text t "class DuckLike a where"
	itext t 1 "duckWalk :: a -> String"
	itext t 1 "quack :: a -> String"
	text t "", \t -> do
	semititle t "このページのことは忘れてください"
 ]

color :: Page
color = [\t -> do
	writeTopTitle t "色々な色"
	text t "色を持つという性質を表すクラス"
	text t "", \t -> do
	text t "data Color = Red | Yellow | Green"
	text t ""
	text t "class Colored a where"
	itext t 1 "getColor :: a -> Color"
 ]

vegetable :: Page
vegetable = [\t -> do
	writeTopTitle t "色々な野菜"
	text t "", \t -> do
	text t "data Vegetable = Tomato | Paprika | Cabage"
	text t ""
	text t "instance Colored Vegetable where"
	itext t 1 "getColor Tomato = Red"
	itext t 1 "getColor Paprika = Yellow"
	itext t 1 "getColor Cabage = Green"
 ]

fruit :: Page
fruit = [\t -> do
	writeTopTitle t "色々な果物"
	text t "", \t -> do
	text t "data Fruit = Apple | Banana | Kiwi"
	text t ""
	text t "instance Colored Fruit where"
	itext t 1 "getColor Apple = Red"
	itext t 1 "getColor Banana = Yellow"
	itext t 1 "getColor Kiwi = Green"
 ]

fortune :: Page
fortune = [\t -> do
	writeTopTitle t "占い"
	text t "", \t -> do
	text t "fortune :: Colored a => a -> IO ()"
	text t "fortune x = putStrLn $ case getColor x of"
	itext t 1 "Red -> \"情熱的な人ですね。\""
	itext t 1 "Yellow -> \"楽天的って言われませんか?\""
	itext t 1 "Green -> \"年のわりに落ち着いてますね。\""
	text t "", \t -> do
	text t "fortune Apple => 情熱的な人ですね。", \t -> do
	text t "fortune Kiwi => 年のわりに落ち着いてますね。"
 ]

naDatabase :: Page
naDatabase = [\t -> do
	writeTopTitle t "名前から年齢を求める"
	text t "", \t -> do
	text t "type Name = String"
	text t "type Age = Int"
	text t ""
	text t "class NADatabase db where"
	itext t 1 "getAge :: Name -> db -> Age"
	text t "", \t -> do
	text t "tellAge :: NADatabase => db -> Name -> db -> IO ()"
	text t "tellAge n db = putStrLn $ n ++ \"さんは\" ++"
	itext t 1 "show getAge db ++ \"歳です\""
 ]

grow :: Page
grow = [\t -> do
	writeTopTitle t "成長する", \t -> do
	text t "class Growable a where"
	itext t 1 "grow :: a -> a"
	itext t 1 "isGoal :: a -> Bool"
	text t "", \t -> do
	text t "data Human = Baby | Child | Adult | Old deriving Show", \t -> do
	text t "instance Growable Human where", \t -> do
	itext t 1 "grow Baby = Child"
	itext t 1 "grow Child = Adult"
	itext t 1 "grow Adult = Old"
	itext t 1 "grow Old = Old", \t -> do
	itext t 1 "isGoal Old = True"
	itext t 1 "isGoal _ = False"
 ]

buri :: Page
buri = [\t -> do
	writeTopTitle t "ブリ"
	text t "", \t -> do
	text t "data Buri = Tsubasu | Hamachi | Mejiro | Buri"
	itext t 1 "deriving Show"
	text t "", \t -> do
	text t "instance Growable Buri where"
	itext t 1 "grow Tsubasu = Hamachi"
	itext t 1 "grow Hamachi = Mejiro"
	itext t 1 "grow Mejiro = Buri"
	itext t 1 "grow Buri = Buri", \t -> do
	itext t 1 "isGoal Buri = True"
	itext t 1 "isGoal _ = False"
 ]

bora :: Page
bora = [\t -> do
	writeTopTitle t "ボラ"
	text t "", \t -> do
	text t "data Bora = Oboko | Subashiri | Ina | Bora | Todo"
	itext t 1 "deriving Show"
	text t "", \t -> do
	text t "instance Growable Bora where"
	itext t 1 "grow Oboko = Subashiri"
	itext t 1 "grow Subashiri = Ina"
	itext t 1 "grow Ina = Bora"
	itext t 1 "grow Bora = Todo"
	itext t 1 "grow Todo = Todo", \t -> do
	itext t 1 "isGoal Todo = True"
	itext t 1 "isGoal _ = False"
 ]

growing :: Page
growing = [\t -> do
	writeTopTitle t "ヒトもサカナも成長する"
	text t "", \t -> do
	text t "grow Baby => Child"
	text t "grow Child => Adult"
	text t "isGoal Adult => True"
	text t "", \t -> do
	text t "grow Tsubasu => Hamachi"
	text t "grow Hamachi => Mejiro"
	text t "isGoal Buri => True"
	text t "", \t -> do
	text t "grow Oboko => Subashiri"
	text t "grow Subashiri => Ina"
	text t "isGoal Todo => True"
 ]

growInt :: Page
growInt = [\t -> do
	writeTopTitle t "整数も成長する"
	text t "", \t -> do
	text t "instance Growable Int where"
	itext t 1 "grow n = n + 1", \t -> do
	itext t 1 "isGoal = (maxBound ==)"
	text t "", \t -> do
	text t "grow 3 => 4"
	text t "grow 111 => 112"
	text t "isGoal 2147483647 => True"
 ]

future :: Page
future = [\t -> do
	writeTopTitle t "将来を表示する関数"
	text t "", \t -> do
	text t "printFuture :: (Growable a, Show a) => IO ()"
	text t "printFuture = do"
	itext t 1 "print x"
	itext t 1 "if isGoal x"
	itext t 2 "then return ()"
	itext t 2 "else printFuture $ grow x"
 ]

futureGrowing1 :: Page
futureGrowing1 = [\t -> do
	writeTopTitle t "将来を表示してみる(1)"
	text t "", \t -> do
	text t "> printFuture Baby"
	text t "Baby"
	text t "Child"
	text t "Adult"
	text t "Old"
	text t "", \t -> do
	text t "> printFuture Tsubasu"
	text t "Tsubasu"
	text t "Hamachi"
	text t "Mejiro"
	text t "Buri"
 ]

futureGrowing2 :: Page
futureGrowing2 = [\t -> do
	writeTopTitle t "将来を表示してみる(2)"
	text t "", \t -> do
	text t "> printFuture Subashiri"
	text t "Subashiri"
	text t "Ina"
	text t "Bora"
	text t "Todo"
	text t "", \t -> do
	text t "> printFuture (2147483644 :: Int)"
	text t "2147483644"
	text t "2147483645"
	text t "2147483646"
	text t "2147483647"
 ]

futureSummary :: Page
futureSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* 型クラスは複数の型に共通する性質を表現する", \t -> do
	text t "* 今回は「成長する」という性質をくくり出した", \t -> do
	text t "* 「Growable =>」という「型制約」をつけた関数のなかで"
	arrowIText t 1 "growとisGoalが使える", \t -> do
	text t "* growとisGoalだけしか使わない関数ならば"
	arrowIText t 1 "Growableのインスタンスである型すべてを扱える"
 ]

queuePrelude :: Page
queuePrelude = [\t -> do
	writeTopTitle t "キュー(はじめに)"
	text t "", \t -> do
	text t "* 続く例題ではキューの話をする", \t -> do
	text t "* C言語やschemeなどではおそらくリンクトリストを使う", \t -> do
	text t "* Haskellのリストはimmutableでキューとしては使えない", \t -> do
	itext t 1 "- 使えるけれど効率の問題がある", \t -> do
	text t "* キューに関してはいくつかの解決策がある", \t -> do
	itext t 1 "- 状態変化を許してIO内でenqueue, dequeueを行う"
	itext t 1 "- ([a], [a])型を使い、償却定数時間とする"
	itext t 1 "- Sequence型を使い、両端キューにも対応する", \t -> do
	text t "* 今回は状態変化を許さず償却定数時間となるキューを扱う"
 ]

queue1 :: Page
queue1 = [\t -> do
	writeTopTitle t "キュー(はじめに)"
	text t "", \t -> do
	text t "* 人工的な例", \t -> do
	itext t 1 "- 現実的な例ではない", \t -> do
	text t "* キューの実装を後回しにする", \t -> do
	itext t 1 "- キューの実装を後からさしかえることができる"
 ]

queue2 :: Page
queue2 = [\t -> do
	writeTopTitle t "キュー(クラス定義)"
	text t "", \t -> do
	text t "class Queue q where"
	itext t 1 "empty :: q a"
	itext t 1 "enqueue :: a -> q a -> q a"
	itext t 1 "dequeue :: q a -> (a, q a)"
 ]

queue3 :: Page
queue3 = [\t -> do
	writeTopTitle t "キュー(リストによる実装)", \t -> do
	text t "instance Queue [] where"
	itext t 1 "empty = []"
	itext t 1 "enqueue x xs = x : xs"
	itext t 1 "dequeue xs = (last xs, init xs)"
	text t "", \t -> do
	text t "last [x] = x"
	text t "last (_ : xs) = last xs"
	text t "init [x] = []"
	text t "init (x : xs) = x : init xs", \t -> do
	text t "* リストを末尾まで走査するのでこれらはO(n)", \t -> do
	text t "* 効率が悪いが簡単", \t -> do
	arrowIText t 0 "とりあえずこれを使っておき、あとでさしかえれば良い"
 ]

queue4 :: Page
queue4 = [\t -> do
	writeTopTitle t "キューを使うプログラム", \t -> do
	text t "getMessage :: Queue q => q String -> IO (q String)"
	text t "getMessage ms = do"
	itext t 1 "m <- getLine"
	itext t 1 "if m == \".\""
	itext t 2 "then return ms"
	itext t 2 "else getMessages $ enqueue m ms", \t -> do
	text t "readMessage :: Queue q => q String -> IO ()"
	text t "readMessage ms = if isEmpty q"
	itext t 1 "then return ()"
	itext t 1 "else do"
	itext t 2 "let (m, ms') = dequeue q"
	itext t 2 "putStrLn m"
	itext t 2 "readMessages ms"
 ]

queue5 :: Page
queue5 = [\t -> do
	writeTopTitle t "キューを使うプログラム"
	text t "", \t -> do
	text t "main :: IO ()"
	text t "main = getMessages (empty :: [String]) >>= readMessages"
 ]

queue6 :: Page
queue6 = [\t -> do
	writeTopTitle t "キュー(償却定数時間)"
	text t "* すこしの工夫で線形時間から償却定数時間にできる"
	text t "", \t -> do
	text t "data TwoList a = TowList [a] [a]"
	text t "instance Queue TwoList"
	itext t 1 "empty = TwoList [] []"
	itext t 1 "isEmpty (TwoList [] []) = True"
	itext t 1 "isEmpty _ = False"
	itext t 1 "enqueue x (TwoList es ds) = TwoList (x : es) ds"
	itext t 1 "dequeue (TwoList es []) ="
	itext t 2 "dequeue (TwoList [] $ reverse es)"
	itext t 1 "dequeue (TwoList es (x : ds)) = (x, TwoList es ds)"
 ]

queue7 :: Page
queue7 = [\t -> do
	writeTopTitle t "キュー(既存のプログラムの変更)"
	text t "* 型注釈を変えるだけで良い"
	text t "", \t -> do
	text t "main :: IO ()"
	text t "main = getMessage (empty :: TwoList String) >>="
	itext t 1 "readMessages"
 ]

defaultMethod :: Page
defaultMethod = [\t -> do
	writeTopTitle t "デフォルトメソッド"
	text t "", \t -> do
	text t "class Eq a => Growable a of"
	itext t 1 "grow :: a -> a"
	itext t 1 "isGoal :: a -> Bool"
	itext t 1 "isGoal x = grow x == grow x"
	text t "", \t -> do
	text t "* isGoalはinstance宣言で定義しなければデフォルトの定義", \t -> do
	text t "* Eq a =>は文脈(==)を使うために必要", \t -> do
	text t "* この定義ではGrowableであるためにはEqである必要がある"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 型クラスは複数の型に共通の性質をくくり出すもの", \t -> do
	text t "* data = ... deriving (...)で簡単にインスタンス化できる", \t -> do
	itext t 1 "- Eq, Show等", \t -> do
	text t "* 見方を変えればインターフェースと実装の分離とも", \t -> do
	text t "* class定義に文脈をつけることができる", \t -> do
	itext t 1 "- このclassのinstanceであるならば...のinstance", \t -> do
	text t "* デフォルトメソッドを用意することもできる"
 ]
