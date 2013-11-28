import Lecture

subtitle :: String
subtitle = "第30回 zipper"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, forWhat, forWhat2, forWhat3,
	listZipper, listZipper2, listZipper3, listZipper4
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 参照透過性を保つことには大きな意味がある", \t -> do
	text t "* それはプログラムが大きくなればなるほど重要になる", \t -> do
	text t "* 参照透過性を保てればそのぶんだけ堅牢な構造が作れる", \t -> do
	text t "* Haskellにはそれを重視する文化がある", \t -> do
	text t "* 状態の変化を必要とするアルゴリズムもある", \t -> do
	itext t 1 "- もちろんHaskellでもそれは実装できる", \t -> do
	text t "* しかし、状態変化無しで同じことができればもっと良い", \t -> do
	text t "* 上手に状態変化を避けられるアルゴリズムのひとつ", \t -> do
	text t "* それがzipperという構造である"
 ]

forWhat :: Page
forWhat = [\t -> do
	writeTopTitle t "何のためのもの?"
	text t "", \t -> do
	text t "* ディレクトリ構造を考えてみよう", \t -> do
	text t "* たとえばこんなことをする", \t -> do
	itext t 1 "% cd memo/birds"
	itext t 1 "% vi tsugumi.txt"
	itext t 1 "% cd ../humans"
	itext t 1 "% vi tarou.txt", \t -> do
	text t "* ディレクトリを移動しながら様々な作業を行っている", \t -> do
	text t "* ここで保存されているのは全体の構造と「今いる場所」"
 ]

forWhat2 :: Page
forWhat2 = [\t -> do
	writeTopTitle t "何のためのもの?"
	text t "", \t -> do
	text t "* 例えばC言語ではどうやる?", \t -> do
	itext t 1 "- 木構造を作りそれを全体の構造とする", \t -> do
	itext t 1 "- ポインタで「今いる場所」を示す", \t -> do
	itext t 1 "- ポインタで示された「もの」を変化させる", \t -> do
	arrowIText t 1 "いつ、どこで、何が変化するかわからない", \t -> do
	text t "* Haskellでは?", \t -> do
	itext t 1 "- 木構造を作りそれを全体の構造とする", \t -> do
	itext t 1 "- ポインタに相当するものはない", \t -> do
	arrowIText t 1 "木構造を作り直す必要がある"
 ]

forWhat3 :: Page
forWhat3 = [\t -> do
	writeTopTitle t "何のためのもの?"
	text t "", \t -> do
	text t "* 木構造を作り直すコスト", \t -> do
	itext t 1 "- 見た目ほどコストは高くない", \t -> do
	itext t 1 "- 簡単のためリストを例に見てみよう", \t -> do
	text t "* リストは一分木と考えることができる", \t -> do
	text t "* [1, 2, 3, 4]という木を[3, 2, 3, 4]に変えるとする", \t -> do
	text t "* 1 : restでrestをパターンマッチして3 : restとする", \t -> do
	text t "* 状態変化がないのでrestの部分は安全に共有できる", \t -> do
	text t "* リストのheadを変化させる", \t -> do
	itext t 1 "- リストのtailを取ってきて", \t -> do
	itext t 1 "- 新しいheadをつけてやれば良い"
 ]

listZipper :: Page
listZipper = [\t -> do
	writeTopTitle t "リストの要素を変化させる"
	text t "", \t -> do
	text t "* 以下のことを考える", \t -> do
	itext t 1 "- リストのいろんな場所に移動", \t -> do
	itext t 1 "- そこの場所の値を変化させる", \t -> do
	text t "* 素直な解法", \t -> do
	itext t 1 "- リストの全体と今いる場所を保持すれば良いので", \t -> do
	itext t 1 "type ListPos a = (Int, [a])"
	itext t 1 "goForward, goBack :: ListPos a -> ListPos a"
	itext t 1 "goForward (h, l) = (h + 1, l)"
	itext t 1 "goBack (h, l) = (h - 1, l)"
	itext t 1 "replaceBy :: a -> ListPos a -> ListPos a"
	itext t 1 "replaceBy x (h, l) = take h l ++ [x] ++"
	itext t 2 "drop (h + 1) l"
 ]

listZipper2 :: Page
listZipper2 = [\t -> do
	writeTopTitle t "リストの要素を変化させる"
	text t "", \t -> do
	text t "* replaceByに注目", \t -> do
	itext t 1 "replaceBy x (h, l) = take h l ++ [x] ++"
	itext t 2 "drop (h + 1) l", \t -> do
	text t "* これは効率が悪い", \t -> do
	itext t 1 "- (h + 1)個の要素を取り除いて再度追加している", \t -> do
	itext t 1 "- hに対してO(N)の時間がかかる", \t -> do
	text t "* もっと効率の良い実装方法がある"
 ]

listZipper3 :: Page
listZipper3 = [\t -> do
	writeTopTitle t "リストの要素を変化させる"
	text t "", \t -> do
	text t "* リストの全体と今いる場所を保持したい", \t -> do
	text t "* 以下の2つのリストの組で表現できる", \t -> do
	itext t 1 "- 「今いる場所」を先頭とするリスト", \t -> do
	itext t 1 "- それ以前に通ってきた履歴のリスト", \t -> do
	itext t 1 "type ListZipper a = ([a], [a])"
	itext t 1 "goForward, goBack :: ListZipper a -> ListZipper a"
	itext t 1 "goForward (x : xs, bs) = (xs, x : bs)"
	itext t 1 "goBack (xs, b : bs) = (b : xs, bs)"
	itext t 1 "replaceBy :: a -> ListZipper a -> ListZipper a"
	itext t 1 "replaceBy x (_ : xs, bs) = (x : xs, bs)", \t -> do
	text t "* これはどの関数もO(1)時間である"
 ]

listZipper4 :: Page
listZipper4 = [\t -> do
	writeTopTitle t "リストの要素を変化させる"
	text t "", \t -> do
	text t "* リストとの相互変換関数", \t -> do
	itext t 1 "fromList :: [a] -> ListZipper a"
	itext t 1 "fromList xs = (xs, [])", \t -> do
	itext t 1 "toList :: ListZipper a -> [a]"
	itext t 1 "toList (xs, bs) = reverse bs ++ xs", \t -> do
	text t "* 使いやすくするために以下の関数を定義する", \t -> do
	itext t 1 "(-:) :: a -> (a -> b) -> b"
	itext t 1 "x -: f = f x"
 ]
