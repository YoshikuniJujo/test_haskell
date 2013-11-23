import Lecture

subtitle :: String
subtitle = "第19回 差分リスト"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, prelude2, prelude3,
	diffList, diffList2, diffList3, diffList4, diffList5,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* リストの結合を考える", \t -> do
	itext t 1 "(++) :: [a] -> [a] -> [a]"
	itext t 1 "[] ++ ys = ys"
	itext t 1 "(x : xs) ++ ys = x : (xs ++ ys)", \t -> do
	text t "* これは一番目のリストの長さに比例した時間がかかる", \t -> do
	text t "* 二番目のリストの長さは処理の時間に関係ない"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 以下の2つを比較してみよう", \t -> do
	itext t 1 "((as ++ bs) ++ cs) ++ ds"
	itext t 1 "as ++ (bs ++ (cs ++ ds))", \t -> do
	text t "* as, bs, cs, dsのそれぞれの長さが3, 4, 9, 2だとする", \t -> do
	text t "* 一番目の計算にかかる時間は以下のようになる", \t -> do
	itext t 1 "3 + (3 + 4) + (3 + 4 + 9) = 26", \t -> do
	text t "* それに対して二番目の計算にかかる時間は", \t -> do
	itext t 1 "3 + 4 + 9 = 16", \t -> do
	text t "* 結果は同じ", \t -> do
	text t "* 効率は大きく変わる", \t -> do
	arrowIText t 1 "リストの結合は常に右結合にしたい"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* しかし、現実には左から順に結合していきたい場面が多い", \t -> do
	text t "* 左から順に結合していっても、結果は右結合にしたい", \t -> do
	arrowIText t 1 "そんな都合のいうことができるの?", \t -> do
	arrowIText t 1 "できるよ!", \t -> do
	dvArrowShort t
	semititle t "それが差分リスト"
 ]

diffList :: Page
diffList = [\t -> do
	writeTopTitle t "差分リストの定義"
	text t "", \t -> do
	semititle t "type DiffList a = [a] -> [a]", \t -> do
	text t "これってリストを取ってリストを返す関数でしょ?", \t -> do
	text t "関数が差分「リスト」なの?", \t -> do
	arrowIText t 1 "関数と値の境界は実は曖昧なのです"
	text t "", \t -> do
	text t "* 差分リストを関数として考えると", \t -> do
	itext t 1 "- リストを入力として取り", \t -> do
	itext t 1 "- 自身の値の後ろに入力値を追加した値を返す関数"
 ]

diffList2 :: Page
diffList2 = [\t -> do
	writeTopTitle t "リストとの変換"
	text t "", \t -> do
	text t "* リストとの相互変換関数を作る", \t -> do
	itext t 1 "fromList :: [a] -> DiffList a"
	itext t 1 "fromList xs = \\x -> xs ++ x", \t -> do
	itext t 1 "toList :: DiffList a -> [a]"
	itext t 1 "toList xs = xs []", \t -> do
	text t "* fromListは(xs ++)とも書ける", \t -> do
	itext t 1 "- リストを、それ自身を何かの前につける関数に変換", \t -> do
	text t "* toListはDiffListが保持している値の後ろに[]をつける", \t -> do
	itext t 1 "- つまり、保持している値を取り出している", \t -> do
	text t "* ここまでは何も面白くない"
 ]

diffList3 :: Page
diffList3 = [\t -> do
	writeTopTitle t "結合関数"
	text t "", \t -> do
	text t "* 差分リストを結合する関数を書く", \t -> do
	text t "* 結合した結果も差分リストになるはずだ", \t -> do
	text t "* 言葉で説明するとこうなる", \t -> do
	itext t 1 "後ろにつける何かを取ってリストを作る関数", \t -> do
	itext t 1 "そのような関数を2つ取って", \t -> do
	itext t 1 "「それぞれの保持する値を結合した値」に", \t -> do
	itext t 1 "入力値を結合する関数", \t -> do
	text t "* 関数定義は以下のようになる", \t -> do
	itext t 1 "append :: DiffList -> DiffList -> DiffList"
	itext t 1 "f `append` g = \\xs -> f (g xs)"
 ]

diffList4 :: Page
diffList4 = [\t -> do
	writeTopTitle t "結合関数"
	text t "", \t -> do
	text t "* \\xs -> f (g xs)がfとgの結合になっている", \t -> do
	text t "* 具体例を入れるとわかりやすい", \t -> do
	text t "* f = (\"hello\" ++)とし、g = (\"world\" ++)とする", \t -> do
	itext t 1 "- すると\\xs -> f (g xs)は以下のようになる", \t -> do
	itext t 2 "\\xs -> \"hello\" ++ (\"workd\" ++ xs)", \t -> do
	text t "* fとgの保持する値を結合したものに入力値を結合する関数", \t -> do
	text t "* そして、次に結合する値はより強く結合している", \t -> do
	itext t 1 "- つまり右結合になる", \t -> do
	text t "* \\xs -> f (g xs)をよく見る", \t -> do
	itext t 1 "- これは単なる関数の結合と同じだ", \t -> do
	itext t 1 "- つまりappend = (.)となる"
 ]

diffList5 :: Page
diffList5 = [\t -> do
	writeTopTitle t "そのままでも"
	text t "", \t -> do
	text t "* わかりやすいように関数を定義した", \t -> do
	itext t 1 "toList, fromList, append", \t -> do
	text t "* でも実用上はもとの関数をそのまま使えば良い", \t -> do
	itext t 1 "(\"hello\" ++) . (\"world\" ++) $ \"\"", \t -> do
	text t "* 上のようにして次々とつなげていけば良い", \t -> do
	itext t 1 "(as ++) . (bs ++) . (cs ++) . (ds ++) $ []", \t -> do
	text t "* (.)を左結合でつないでも出来上がるリストは右結合になる", \t -> do
	itext t 1 "(((as ++) . (bs ++)) . (cs ++)) . (ds ++) $ []"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 差分リストについて学んだ", \t -> do
	text t "* リストの結合は右結合のほうが良い", \t -> do
	text t "* 右結合であることを強制するようなアルゴリズムがある", \t -> do
	text t "* それが差分リストを使う方法である", \t -> do
	text t "* 最もシンプルに本質を書けば以下のようになる", \t -> do
	itext t 1 "(as ++) . (bs ++) . (cs ++) . (ds ++) $ []"
 ]
