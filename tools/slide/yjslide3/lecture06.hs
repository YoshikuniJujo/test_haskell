import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第6回 再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutSquareSum, aboutSquareSum2, aboutSquareSum3, aboutSquareSum4,
	aboutSquareSum5, aboutSquareSum6, aboutSquareSum7, aboutSquareSum8,
	aboutSquareSum9, aboutSquareSum10, aboutSquareSum11, aboutSquareSumSummary,
	treeRec, treeRec2, treeRec3, treeRec4, treeRec5, treeRec6, treeRec7,
	treeRec8, treeRec9, treeRec10, treeRec11, treeRec12, treeRec13,
	treeRec14, treeRec15, treeRec16,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 再帰関数は関数型言語の魂", \t -> do
	text t "* 「くりかえし」を含むさまざまな処理の流れが表現可能", \t -> do
	text t "* 前回のリストを処理する関数群も再帰的に定義されている", \t -> do
	text t "* 非常に強力な道具", \t -> do
	text t "* 再帰関数を使えば、ほとんど「何でもできる」が", \t -> do
	text t "* 直接的に再帰関数を使う前に", \t -> do
	text t "* より力の弱い方法で実現できないか考えたほうがいい", \t -> do
	itext t 1 "- 再帰を使うのではなくmapとfilterを使う、等", \t -> do
	text t "* そのほうがプログラムがわかりやすくなる場合がある"
 ]

aboutSquareSum :: Page
aboutSquareSum = [\t -> do
	writeTopTitle t "二乗の和"
	text t "", \t -> do
	text t "* 「リスト」の回でやった二乗の和を再帰で見てみよう", \t -> do
	text t "* 前回のやりかたのほうがプログラムはわかりやすいので", \t -> do
	itext t 1 "この例の場合は前回のようにリストを使うほうが良い", \t -> do
	text t "* 今回のやりかたを見ることによって", \t -> do
	itext t 1 "- より低レベルの「仕組み」が理解できる", \t -> do
	itext t 1 "- より広い範囲に応用できる"
 ]

aboutSquareSum2 :: Page
aboutSquareSum2 = [\t -> do
	writeTopTitle t "問題定義"
	text t "", \t -> do
	text t "* 与えられた引数nに対して0からnまでの二乗の和を求める", \t -> do
	text t "* リストを使った解", \t -> do
	itext t 1 "1. 0からnまでのリストがあるとする", \t -> do
	itext t 1 "2. そのすべての要素を二乗したリストがあれば", \t -> do
	itext t 1 "3. そのリストの要素の総和が求める値である", \t -> do
	text t "* 直接的に再帰を使った解", \t -> do
	itext t 1 "1. 0から0までの二乗の和は0である", \t -> do
	itext t 1 "2. 0から(n - 1)までの二乗の和Sがあれば", \t -> do
	itext t 1 "3. 0からnまでの二乗の和はn ^ 2 + S"
 ]

aboutSquareSum3 :: Page
aboutSquareSum3 = [\t -> do
	writeTopTitle t "関数定義"
	text t "", \t -> do
	text t "* 1. 0から0までの二乗の和は0である", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	text t "* 2. 0から(n - 1)までの二乗の和Sがあれば", \t -> do
	itext t 1 "SはsquareSum (n - 1)", \t -> do
	text t "* 3. 0からnまでの二乗の和はn ^ 2 + S", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	text t "* よって以下のようになる", \t -> do
	itext t 1 "squareSum :: Int -> Int", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)"
 ]

squareSum :: Int -> Int
squareSum 0 = 0
squareSum n = n ^ (2 :: Int) + squareSum (n - 1)

ss4int1 :: Int
ss4int1 = unsafePerformIO $ randomRIO (3, 8)

aboutSquareSum4 :: Page
aboutSquareSum4 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* より詳細な説明をする前に実際に動かしてみよう", \t -> do
	text t "* lectures/lecture06ディレクトリを作成しそこに移動", \t -> do
	text t "* コマンドプロンプトを2こ立ち上げて", \t -> do
	itext t 1 "- エディタでsquareSum.hsを作成しよう", \t -> do
	itext t 2 "squareSum :: Int -> Int", \t -> do
	itext t 2 "squareSum 0 = 0", \t -> do
	itext t 2 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	itext t 1 "- ghci squareSum.hsで読み込もう", \t -> do
	itext t 1 $ "*Main> squareSum " ++ show ss4int1, \t -> do
	itext t 1 $ show $ squareSum ss4int1
 ]

aboutSquareSum5 :: Page
aboutSquareSum5 = [\t -> do
	writeTopTitle t "どうしてこれが動くの?"
	text t "", \t -> do
	text t "* 定義をもう一度見てみよう", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	text t "* 順を追って見ていく", \t -> do
	itext t 1 "n = 1", \t -> do
	itext t 1 "squareSum 1 = 1 ^ 2 + squareSum 0", \t -> do
	itext t 1 "squareSum 1 = 1", \t -> do
	itext t 1 "n = 2", \t -> do
	itext t 1 "squareSum 2 = 2 ^ 2 + squareSum 1", \t -> do
	itext t 1 "squareSum 2 = 4 + 1", \t -> do
	itext t 1 "squareSum 2 = 5"
 ]

aboutSquareSum6 :: Page
aboutSquareSum6 = [\t -> do
	writeTopTitle t "ひとつ前を使う"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "n = 0の値を使ってn = 1の値を求め", \t -> do
	itext t 1 "n = 1の値を使ってn = 2の値を求め", \t -> do
	itext t 1 "n = 2の値を使ってn = 3の値を求め", \t -> do
	itext t 2 "..."
	itext t 1 "n = k - 1の値を使ってn = kの値を求める", \t -> do
	itext t 2 "...", \t -> do
	text t "* よってnが0以上のすべての場合について値が求まる"
 ]

aboutSquareSum7 :: Page
aboutSquareSum7 = [\t -> do
	writeTopTitle t "squareSumの意味"
	text t "", \t -> do
	text t "* もう一度定義を見てみよう", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	text t "* これは以下のように読める", \t -> do
	itext t 1 "0から0の値の二乗の和は0", \t -> do
	itext t 1 "0からnの値の二乗の和は", \t -> do
	itext t 2 "0から(n - 1)の値の二乗の和にn ^ 2を足したもの", \t -> do
	text t "* 動作ではなく「事実」を記述している", \t -> do
	text t "* 動かさなくても字面からプログラムの意味がわかる"
 ]

aboutSquareSum8 :: Page
aboutSquareSum8 = [\t -> do
	writeTopTitle t "再帰関数を作るコツ"
	text t "", \t -> do
	text t "* まずは最もシンプルな部分を作る", \t -> do
	itext t 1 "これを基底と呼ぶ", \t -> do
	text t "* その関数自体がすでに定義されていると考えて", \t -> do
	itext t 1 "より基底に近い定義から次の値を定義する", \t -> do
	text t "* squareSumの場合で考えてみる", \t -> do
	itext t 1 "- 引数0が基底となる", \t -> do
	itext t 1 "- squareSumがすでに定義ずみと考えて", \t -> do
	itext t 2 "squareSum (n -1)を使ってsquareSum nを定義する", \t -> do
	text t "* 重要なのはnより(n - 1)のほうが0に近いこと", \t -> do
	itext t 1 "1を引くという操作をくりかえすと0に到達する"
 ]

aboutSquareSum9 :: Page
aboutSquareSum9 = [\t -> do
	writeTopTitle t "squareSumの評価"
	text t "", \t -> do
	text t "* 「再帰」を理解できればHaskellの半分は理解できたことに", \t -> do
	text t "* 理解のために様々な側面から説明する", \t -> do
	text t "* squareSumの逐次的な評価を見てみよう", \t -> do
	itext t 1 "squareSum 3", \t -> do
	arrowIText t 1 "3 ^ 2 + squareSum 2", \t -> do
	arrowIText t 1 "9 + (2 ^ 2 + squareSum 1)", \t -> do
	arrowIText t 1 "9 + (4 + (1 ^ 2 + squareSum 0))", \t -> do
	arrowIText t 1 "9 + (4 + (1 + 0))", \t -> do
	arrowIText t 1 "14"
 ]

aboutSquareSum10 :: Page
aboutSquareSum10 = [\t -> do
	writeTopTitle t "再帰関数を理解するコツ"
	text t "", \t -> do
	text t "* 再帰関数を見たら2つの方法で理解を試みる", \t -> do
	itext t 1 "- 字面から「何であるか」をとらえようとする", \t -> do
	itext t 1 "- 簡単なケースで逐次的な評価を書きくだす", \t -> do
	text t "* この2つの方法を試すことで仮説を立てることができる", \t -> do
	text t "* 何度かの試行錯誤が必要", \t -> do
	text t "* その関数が何であるか仮説を立てたらそれを検証する"
 ]

aboutSquareSum11 :: Page
aboutSquareSum11 = [\t -> do
	writeTopTitle t "仮説を検証する"
	text t "", \t -> do
	text t "* squareSumの定義を読んで以下の仮説を立てたとする", \t -> do
	itext t 1 "squareSum nは0からnまでの値の二乗の和である", \t -> do
	text t "* 関数の定義がその仮説と合致することを確認する", \t -> do
	itext t 1 "squareSum 0 = 0", \t -> do
	arrowIText t 2 "0のとき仮説は正しい", \t -> do
	itext t 1 "squareSum n = n ^ 2 + squareSum (n - 1)", \t -> do
	itext t 2 "- 0からnまでの二乗の和 ... (1)", \t -> do
	itext t 2 "- n ^ 2 + 0から(n - 1)までの二乗の和 ... (2)", \t -> do
	itext t 2 "- (1)と(2)は等しい", \t -> do
	text t "* よって仮説と定義は矛盾しない"
 ]

aboutSquareSumSummary  :: Page
aboutSquareSumSummary = [\t -> do
	writeTopTitle t "squareSum(まとめ)"
	text t "", \t -> do
	text t "* squareSumを例に", \t -> do
	itext t 1 "- 再帰関数の定義のしかたを見た", \t -> do
	itext t 1 "- 再帰関数の定義から意味を理解するやりかたを見た", \t -> do
	text t "* 再帰関数を作るときは", \t -> do
	itext t 1 "- 最もシンプルな基底ケースを書く", \t -> do
	itext t 1 "- 「次」の定義を「前」の定義から導く", \t -> do
	text t "* 再帰関数を読むときは", \t -> do
	itext t 1 "- 「字面を見る」や「逐次的評価」によって仮説を立て", \t -> do
	itext t 1 "- 仮説が定義と矛盾しないことを検証する"
 ]

treeRec :: Page
treeRec = [\t -> do
	writeTopTitle t "より複雑な再帰"
	text t "", \t -> do
	text t "* 再帰的定義で「くりかえし」より複雑な制御の流れを定義可", \t -> do
	text t "* 制御の流れが「木構造」となる関数を見ていこう"
 ]

treeRec2 :: Page
treeRec2  = [\t -> do
	writeTopTitle t "問題定義"
	text t "", \t -> do
	text t "* 以下のような木を考える", \t -> do
	writeTree t (: "") 15 4 200 110 pathTree
	rtGoto t 200 300, \t -> do
	text t "* ある節から別の節への経路があるかどうかを返す関数", \t -> do
	text t "* そのような関数existPathを考えていこう"
 ]

pathTree :: BinTree Char
pathTree = Bin 'a'
	(Bin 'b'
		(Bin 'd' Empty Empty)
		(Bin 'e' (Bin 'f' Empty Empty) (Bin 'g' Empty Empty)))
	(Bin 'c' Empty Empty)

treeRec3 :: Page
treeRec3 = [\t -> do
	writeTopTitle t "新たに必要になる構文"
	text t "", \t -> do
	text t "* 関数のガード節", \t -> do
	itext t 1 "- 引数の条件によって式を選ぶことができる", \t -> do
	itext t 1 "fun x"
	preLine t
	itext t 2 "| even x = \"even\"", \t -> do
	itext t 2 "| otherwise = \"odd\"", \t -> do
	text t "* case式", \t -> do
	itext t 1 "- 引数部分以外でパターンマッチが使える", \t -> do
	itext t 1 "fun n = case n `mod` 3 of", \t -> do
	itext t 2 "0 -> \"3でわりきれる\"", \t -> do
	itext t 2 "_ -> \"3でわりきれない"
 ]

treeRec4 :: Page
treeRec4 = [\t -> do
	writeTopTitle t "新たに必要になる構文"
	text t "", \t -> do
	text t "* タイプシノニム", \t -> do
	itext t 1 "type [型名] = [型の表現]", \t -> do
	itext t 1 "- 型の別名をつくることができる", \t -> do
	itext t 1 "- [型名]のところに[型の表現]が書かれているのと同じ", \t -> do
	itext t 1 "- プログラムの意味を読む人にわかりやすくする", \t -> do
	itext t 1 "- 長い型を記述する手間を減らす", \t -> do
	itext t 1 "- 例:"
	itext t 2 "type Name = String", \t -> do
	itext t 2 "type Age = Int", \t -> do
	itext t 2 "type HumanList = [(Name, Age)]"
 ]

treeRec5 :: Page
treeRec5 = [\t -> do
	writeTopTitle t "新たに必要になる型"
	text t "", \t -> do
	text t "* Maybe型", \t -> do
	itext t 1 "- Maybe Intという型は以下の値を持つ", \t -> do
	itext t 2 "Just [整数], Nothing", \t -> do
	itext t 1 "- 失敗する可能性のある計算に使われる", \t -> do
	itext t 1 "- 計算が成功した場合", \t -> do
	itext t 2 "Just [結果]", \t -> do
	itext t 1 "- 計算が失敗した場合", \t -> do
	itext t 2 "Nothing"
 ]

treeRec6 :: Page
treeRec6 = [\t -> do
	writeTopTitle t "新たに必要になる関数"
	text t "", \t -> do
	text t "* lookup :: a -> [(a, b)] -> Maybe b", \t -> do
	itext t 1 "- lookup x lstとすると", \t -> do
	itext t 2 "lstの要素である対のなかに", \t -> do
	itext t 2 "対の一番目がxであるようなものがあれば", \t -> do
	itext t 2 "その対の二番目の値を返し", \t -> do
	itext t 2 "なければNothingを返す", \t -> do
	text t "* (||) :: Bool -> Bool -> Bool", \t -> do
	itext t 1 "- 2つのBool値の論理和(または)を返す関数", \t -> do
	itext t 1 "- b1 || b2はb1かb2のどちらかがTrueならTrue"
 ]

treeRec7 :: Page
treeRec7 = [\t -> do
	writeTopTitle t "細かい条件等"
	text t "", \t -> do
	writeTree t (: "") 15 4 200 70 pathTree
	rtGoto t 200 280, \t -> do
	text t "* 木は下向きにしかたどれない", \t -> do
	text t "* 子要素は0または2個"
 ]

treeRec8 :: Page
treeRec8 = [\t -> do
	writeTopTitle t "木の表現"
	text t "", \t -> do
	writeTree t (: "") 15 2 400 90 pathTree
	rtGoto t 400 70, \t -> do
	text t "* 親から子への経路の集合として表現", \t -> do
	text t "* 節点は一文字で表現する", \t -> do
	text t "type Paths = [(Char, (Char, Char))]", \t -> do
	text t "* 例題の木は以下のようになる", \t -> do
	text t "paths :: Paths", \t -> do
	text t "paths = [", \t -> do
	itext t 1 "('a', ('b', 'c')),"
	itext t 1 "('b', ('d', 'e')),"
	itext t 1 "('e', ('f', 'g'))]", \t -> do
	text t "* これらをtreeRec.hsに書き込もう"
 ]

type Paths = [(Char, (Char, Char))]

paths :: Paths
paths = [
	('a', ('b', 'c')),
	('b', ('d', 'e')),
	('e', ('f', 'g'))]

treeRec9 :: Page
treeRec9 = [\t -> do
	writeTopTitle t "見てみよう"
	text t "", \t -> do
	text t "* ghci treeRec.hsで読み込んで", \t -> do
	itext t 1 "*Main> paths", \t -> do
	itext t 1 $ show paths, \t -> do
	text t "* ちゃんと定義されているのがわかる", \t -> do
	text t "* このpathsの意味は以下のようになる", \t -> do
	itext t 1 "- 'a'からは'a' -> 'b', 'a' -> 'c'という経路があり", \t -> do
	itext t 1 "- 'b'からは'b' -> 'd', 'b' -> 'e'という経路があり", \t -> do
	itext t 1 "- 'e'からは'e' -> 'f', 'e' -> 'g'という経路がある"
 ]

treeRec10 :: Page
treeRec10 = [\t -> do
	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* 経路が存在するかどうかを調べる関数existPathを作ろう", \t -> do
	text t "* できあがったときには以下のような動作をするはずだ", \t -> do
	writeTree t (: "") 15 2 400 130 pathTree
	rtGoto t 400 130, \t -> do
	text t "existPath paths 'b' 'f' => True", \t -> do
	text t "existPath paths 'c' 'g' => False", \t -> do
	text t "existPath paths 'f' 'b' => False"
 ]

treeRec11 :: Page
treeRec11 = [\t -> do
	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* まずは型を使めよう", \t -> do
	text t "* 使用例から明らかなように入力、出力は以下のようになる", \t -> do
	itext t 1 "入力1: Paths", \t -> do
	itext t 1 "入力2: Char", \t -> do
	itext t 1 "入力3: Char", \t -> do
	itext t 1 "出力: Bool", \t -> do
	arrowIText t 1 "existPath :: Paths -> Char -> Char -> Bool"
 ]

treeRec12 :: Page
treeRec12 = [\t -> do
 	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* 中身を考えていこう", \t -> do
	text t "* 再帰的に定義することを考える", \t -> do
	text t "* 基底ケースは何だろうか?", \t -> do
	text t "* 最も簡単なケースとして始点と終点が同じ場合が考えられる", \t -> do
	text t "* この場合は経路は存在すると考えてよいので", \t -> do
	itext t 1 "existPath ps b e", \t -> do
	itext t 2 "| b == e = True"
 ]

treeRec13 :: Page
treeRec13 = [\t -> do
 	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* もうひとつの基底ケースがある", \t -> do
	text t "* bとeが同一でなく、しかもbからの経路がない場合", \t -> do
	text t "* この場合は経路は存在しないと考えられるので", \t -> do
	itext t 1 "existPath ps b e", \t -> do
	itext t 2 "| b == e = True", \t -> do
	itext t 2 "| otherwise = case lookup b ps of", \t -> do
	itext t 3 "Nothing -> False", \t -> do
	text t "* bからの経路がないということは以下と同じこと", \t -> do
	text t "* psのなかにbを一番目の要素とするタプルが存在しない", \t -> do
	text t "* よってlookup b psはNothingを返す"
 ]

treeRec14 :: Page
treeRec14 = [\t -> do
	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* 残る可能性は以下の通り", \t -> do
	itext t 1 "- bとeが同一ではなくbからの経路が存在する", \t -> do
	text t "* bのふたつの直接の子のどちらかから", \t -> do
	itext t 1 "- eへの経路が存在すればbからeへの経路は存在し", \t -> do
	itext t 1 "- そうでなければbからeへの経路は存在しない", \t -> do
	text t "* つまりbからeへの経路が存在するというBool値は", \t -> do
	itext t 1 "- bの子1からeへの経路が存在するというBool値と", \t -> do
	itext t 1 "- bの子2からeへの経路が存在するというBool値", \t -> do
	itext t 0.5 "との論理和となる"
 ]

treeRec15 :: Page
treeRec15 = [\t -> do
	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* よって以下のようになる", \t -> do
	itext t 0 "existPath :: Paths -> Char -> Char -> Bool", \t -> do
	itext t 0 "existPath ps b e", \t -> do
	itext t 1 "| b == e = True", \t -> do
	itext t 1 "| otherwise = case lookup b ps of", \t -> do
	itext t 2 "Nothing -> False", \t -> do
	itext t 2 "Just (c1, c2) -> existPath ps c1 e ||"
	itext t 5 "existPath ps c2 e", \t -> do
	text t "* これをtreeRec.hsに書き込もう", \t -> do
	itext t 1 "(1分)"
 ]

existPath :: Paths -> Char -> Char -> Bool
existPath ps b e
	| b == e = True
	| otherwise = case lookup b ps of
		Nothing -> False
		Just (c1, c2) -> existPath ps c1 e || existPath ps c2 e

treeRec16 :: Page
treeRec16 = [\t -> do
	writeTopTitle t "経路の存在関数"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	writeTree t (: "") 15 2 420 80 pathTree
	rtGoto t 400 110, \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> existPath paths 'b' 'f'", \t -> do
	itext t 1 $ show $ existPath paths 'b' 'f', \t -> do
	itext t 1 "*Main> existPath paths 'c' 'g'", \t -> do
	itext t 1 $ show $ existPath paths 'c' 'g', \t -> do
	itext t 1 "*Main> existPath paths 'f' 'b'", \t -> do
	itext t 1 $ show $ existPath paths 'f' 'b', \t -> do
	text t "* 木を下にしかたどれないという条件を思い出そう", \t -> do
	text t "* 関数は正しい結果を返していることがわかる"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 再帰関数の定義のしかた/理解のしかたについて学んだ", \t -> do
	text t "* 定義する際には", \t -> do
	itext t 1 "- 基底ケースを見つける", \t -> do
	itext t 1 "- 基底に近い場合の値からより離れた場合の値を導く", \t -> do
	text t "* 理解する際には", \t -> do
	itext t 1 "- それが「何であるか」をよく見る", \t -> do
	itext t 1 "- 逐次的に評価してみる", \t -> do
	itext t 1 "- 仮説を立てて検証する", \t -> do
	text t "* 例題として二乗の総和のケースを見た", \t -> do
	text t "* 処理が木構造となる例として", \t -> do
	itext t 1 "- 木のなかで経路の存在の有無を返す関数を見た"
 ]
