import Lecture

subtitle :: String
subtitle = "第8回 リストを作成する再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	enumerate, aboutEnumFromTo, aboutEnumFromTo2, aboutEnumFromTo3,
	aboutEnumFromToSummary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回はリストを引数に取る再帰関数について学んだ", \t -> do
	text t "* 今回はリストを返す再帰関数について学ぶ", \t -> do
	text t "* 「リスト」の回でやった3段階は", \t -> do
	itext t 1 "1. enumerate", \t -> do
	itext t 1 "2. map/filter", \t -> do
	itext t 1 "3. accumulate", \t -> do
	text t "* そのうちの1. enumerateにあたる部分となる"
 ]

enumerate :: Page
enumerate = [\t -> do
	writeTopTitle t "[m .. n]"
	text t "", \t -> do
	text t "* まずは「リスト」の回で見た[m .. n]という構文を見よう", \t -> do
	text t "* これは構文糖であり以下の関数に変換される", \t -> do
	itext t 1 "enumFromTo x y", \t -> do
	text t "* この関数を正しく理解するには型クラスの知識が必要なので", \t -> do
	text t "* ここではこの関数の型を以下のように考える", \t -> do
	itext t 1 "enumFromTo :: Int -> Int -> [Int]"
 ]

aboutEnumFromTo :: Page
aboutEnumFromTo = [\t -> do
	writeTopTitle t "enumFromTo"
	text t "", \t -> do
	text t "* enumFromTo m nはmから1ずつ増加させnまでの整数を返す関数", \t -> do
	text t "* 以下のように定義することができる", \t -> do
	itext t 1 "enumFromTo m n", \t -> do
	itext t 2 "| m > n = []", \t -> do
	itext t 2 "| otherwise = m : enumFromTo (m + 1) n"
 ]

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo m n
	| m > n = []
	| otherwise = m : myEnumFromTo (m + 1) n

aboutEnumFromTo2 :: Page
aboutEnumFromTo2 = [\t -> do
	writeTopTitle t "試してみよう"
	text t "", \t -> do
	text t "* コマンドプロンプトを2こ立ち上げて", \t -> do
	text t "* lectures/lecture08ディレクトリを作成しそこに移動", \t -> do
	text t "* myList.hsを作成し以下を書き込もう", \t -> do
	itext t 1 "myEnumFromTo :: Int -> Int -> [Int]", \t -> do
	itext t 1 "myEnumFromTo m n", \t -> do
	itext t 2 "| m > n = []", \t -> do
	itext t 2 "| otherwise = m : enumFromTo (m + 1) n", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "% ghci myList.hs", \t -> do
	itext t 1 "*Main> myEnumFromTo 3 8", \t -> do
	itext t 1 $ show $ myEnumFromTo 3 8
 ]

aboutEnumFromTo3 :: Page
aboutEnumFromTo3 = [\t -> do
	writeTopTitle t "myEnumFromToの説明"
	text t "", \t -> do
	text t "* 定義を再掲する", \t -> do
	itext t 1 "myEnumFromTo m n"
	itext t 2 "| m > n = []"
	itext t 2 "| otherwise = m : enumFromTo (m + 1) n", \t -> do
	text t "* これはこう読める", \t -> do
	itext t 1 "1. 開始の値が終了の値より大きければ空リスト", \t -> do
	itext t 1 "2. mからnまでの値のリストは", \t -> do
	itext t 2 "(m + 1)からnまでの値のリストにmを追加したもの"
 ]

aboutEnumFromToSummary :: Page
aboutEnumFromToSummary = [\t -> do
	writeTopTitle t "enumFromTo(まとめ)"
	text t "", \t -> do
	text t "* [m .. n]という構文は構文糖であり", \t -> do
	itext t 1 "[m .. n]", \t -> do
	arrowIText t 2 "enumFromTo m n", \t -> do
	text t "* enumFromTo m nは以下のように定義される", \t -> do
	itext t 1 "1. m > nならば空リスト", \t -> do
	itext t 1 "2. m + 1からnまでのリストにmを追加したもの", \t -> do
	text t "* その関数自体の返り値に値を追加するという枠組み"
 ]

collatz :: Page
collatz = [\t -> do
	writeTopTitle t ""
 ]
