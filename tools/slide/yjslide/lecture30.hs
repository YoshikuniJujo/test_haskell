import Lecture

subtitle :: String
subtitle = "第30回 zipper"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, forWhat, forWhat2, forWhat3,
	listZipper, listZipper2, listZipper3, listZipper4, listZipper5,
	whatsZipper,
	treeZipper, treeZipper2, treeZipper3, treeZipper4, treeZipper5,
	treeZipper6, treeZipper7, treeZipper8, treeZipper9, treeZipper10,
	maybeZipper, maybeZipper2, maybeZipper3,
	summary
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

listZipper5 :: Page
listZipper5 = [\t -> do
	writeTopTitle t "リストの要素を変化させる"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "test = toList $ fromList [1, 2, 3, 4, 5, 6]"
	itext t 2 "-: goForward"
	itext t 2 "-: replaceBy 99"
	itext t 2 "-: goForward"
	itext t 2 "-: goForward"
	itext t 2 "-: replaceBy 222"
	itext t 2 "-: goBack"
	itext t 2 "-: replaceBy 123", \t -> do
	itext t 1 "> test"
	itext t 1 "[1,99,123,222,5,6]"
 ]

whatsZipper :: Page
whatsZipper = [\t -> do
	writeTopTitle t "zipperとは?"
	text t "", \t -> do
	text t "* 全体の構造と今いる場所を表現したい", \t -> do
	text t "* 素直なやりかただと([全体], [場所])となる", \t -> do
	itext t 1 "- アクセスは毎回トップからたどる必要がある", \t -> do
	text t "* ([残り], [履歴])のようにする", \t -> do
	itext t 1 "- アクセスする場所が表面に出てきている", \t -> do
	text t "* 全体の構造と今いる場所を効率的に保存する手法"
 ]

treeZipper :: Page
treeZipper = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 二分木のzipperを考えてみよう", \t -> do
	text t "* 二分木を定義する", \t -> do
	itext t 1 "data Tree a = Empty | Node a (Tree a) (Tree a)"
	itext t 2 "deriving Show", \t -> do
	text t "* これのzipperを考える", \t -> do
	text t "* 残りの構造はTree aそのもので表すことができる", \t -> do
	text t "* 履歴はどうやって表現しようか?", \t -> do
	itext t 1 "- 「残りの構造」と「履歴」でもとの構造を復元する", \t -> do
	itext t 1 "- 必要な情報は左右どちらに進んだかと", \t -> do
	itext t 1 "- 進んだことによって失われた情報"
 ]

treeZipper2 :: Page
treeZipper2 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* Node x l rという構造について考える", \t -> do
	text t "* 左に進んだ場合、「残りの構造」はlとなる", \t -> do
	text t "* すると失われる情報はxの値と右の木のrである", \t -> do
	text t "* よって、「履歴」に含まれる情報は以下の3つとなる", \t -> do
	itext t 1 "- 進んだ方向", \t -> do
	itext t 1 "- もとの木の値", \t -> do
	itext t 1 "- 進まなかった方向の木", \t -> do
	itext t 1 "data Dir = Left | Right deriving Show"
	itext t 1 "data Log a = Log Dir a (Tree a) deriving Show", \t -> do
	text t "* zipperは木と、履歴のリスト", \t -> do
	itext t 1 "type Zipper a = (Tree a, [Log a])"
 ]

treeZipper3 :: Page
treeZipper3 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 左右に進む関数", \t -> do
	itext t 0.5 "goLeft, goRight :: Zipper a -> Zipper a"
	itext t 0.5 "goLeft (Node x l r, lgs) = (l, Log Left x r : lgs)"
	itext t 0.5 "goRight (Node x l r, lgs) = (r, Log Right x l : lgs)", \t -> do
	text t "* 上にもどる関数", \t -> do
	itext t 0.5 "goUp :: Zipper a -> Zipper a"
	itext t 0.5 "goUp (t, Log Left x r : lgs) = (Node x t r, lgs)"
	itext t 0.5 "goUp (t, Log Right x l : lgs) = (Node x l t, lgs)"
 ]

treeZipper4 :: Page
treeZipper4 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 木の頂上までもどる関数", \t -> do
	itext t 1 "topMost :: Zipper a -> Zipper a"
	itext t 1 "topMost z@(_, []) = z"
	itext t 1 "topMost z = topMost $ goUp z", \t -> do
	text t "* 二分木とzipperの相互変換", \t -> do
	itext t 1 "fromTree :: Tree a -> Zipper a"
	itext t 1 "fromTree t = (t, [])", \t -> do
	itext t 1 "toTree :: Zipper a -> Tree a"
	itext t 1 "toTree = fst . topMost"
 ]

treeZipper5 :: Page
treeZipper5 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 今いる場所の値を設定する関数", \t -> do
	itext t 1 "put :: a -> Zipper a -> Zipper a"
	itext t 1 "put x (Node _ l r, lgs) = (Node x l r, lgs)"
	itext t 1 "put x (Empty, lgs) = (Node x Empty Empty, lgs)", \t -> do
	text t "* 木を作り上げていくのにも使えるように", \t -> do
	itext t 1 "- 空の木にputすると木を作るようにした", \t -> do
	text t "* 今いる場所の値を変化させる関数", \t -> do
	itext t 1 "modify :: (a -> a) -> Zipper a -> Zipper a"
	itext t 1 "modify f (Node x l r, lgs) = (Node (f x) l r, lgs)"
	itext t 1 "modify _ (Empty, lgs) = (Empty, lgs)"
 ]

treeZipper6 :: Page
treeZipper6 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 使いやすいように以下の関数を定義", \t -> do
	itext t 1 "(-:) :: a -> (a -> b) -> b"
	itext t 1 "x -: f = f x", \t -> do
	text t "* 空かどうかをチェックする関数", \t -> do
	itext t 1 "empty :: Tree a -> Bool"
	itext t 1 "empty Empty = True"
	itext t 1 "empty _ = False"
 ]

treeZipper7 :: Page
treeZipper7 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 簡単な木の表示関数を作る", \t -> do
	itext t 0 "showTree :: Show a => Int -> Tree a -> String"
	itext t 0 "showTree i (Node x l r) ="
	itext t 1 "replicate (i * 4) ' ' ++ show x ++ \"\\n\" ++"
	itext t 1 "(if empty l then \"\" else showTree (i + 1) l) ++"
	itext t 1 "(if empty r then \"\" else showTree (i + 1) r)", \t -> do
	itext t 0 "printTree :: Show a => Tree a -> IO ()"
	itext t 0 "printTree = putStr . showTree 0"
 ]

treeZipper8 :: Page
treeZipper8 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* テスト用の木の作成", \t -> do
	itext t 1 "testTree :: Tree String"
	itext t 1 "testTree = toTree $ fromTree Empty"
	itext t 2 "-: put \"life\""
	itext t 2 "-: goLeft -: put \"plant\""
	itext t 2 "-: goLeft -: put \"tree\""
	itext t 2 "-: goUp -: goRight -: put \"grass\""
	itext t 2 "-: goUp -: goUp -: goRight -: put \"animal\""
	itext t 2 "-: goLeft -: put \"human\""
	itext t 2 "-: goUp -: goRight -: put \"dog\""
 ]

treeZipper9 :: Page
treeZipper9 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 見てみる", \t -> do
	itext t 1 "> printTree testTree"
	itext t 1 "\"life\""
	itext t 1.5 "\"plant\""
	itext t 2 "\"tree\""
	itext t 2 "\"grass\""
	itext t 1.5 "\"animal\""
	itext t 2 "\"human\""
	itext t 2 "\"dog\""
 ]

treeZipper10 :: Page
treeZipper10 = [\t -> do
	writeTopTitle t "二分木のzipper"
	text t "", \t -> do
	text t "* 使ってみる", \t -> do
	itext t 1 "> printTree $ toTree $ fromTree testTree"
	itext t 2 "-: goRight -: goLeft"
	itext t 2 "-: modify (++ \" being\")"
	itext t 1 "\"life\""
	itext t 1.5 "\"plant\""
	itext t 2 "\"tree\""
	itext t 2 "\"grass\""
	itext t 1.5 "\"animal\""
	itext t 2 "\"human being\""
	itext t 2 "\"dog\""
 ]

maybeZipper :: Page
maybeZipper = [\t -> do
	writeTopTitle t "失敗する可能性"
	text t "", \t -> do
	text t "* goLeft, goRightは現在地がEmptyのとき失敗する", \t -> do
	text t "* goUpは現在地が頂上のとき失敗する", \t -> do
	text t "* 現在の実装では失敗のときは例外を投げて終了する", \t -> do
	text t "* もっとクリーンな実装にしたい", \t -> do
	itext t 1 "- Maybeを使う"
 ]

maybeZipper2 :: Page
maybeZipper2 = [\t -> do
	writeTopTitle t "失敗する可能性"
	text t "", \t -> do
	text t "* goLeftとgoUpの実装を見る(goRightはgoLeftとほぼ同じ)", \t -> do
	itext t 0 "goLeft, goUp :: Zipper a -> Maybe (Zipper a)"
	itext t 0 "goLeft (Node x l r, lgs) = Just (l, Log Left x r : lgs)"
	itext t 0 "goLeft _ = Nothing"
	itext t 0 "goUp (t, Log Left x r : lgs) = Just (Node x t r, lgs)"
	itext t 0 "goUp (t, Log Right x r : lgs) = Just (Node x l t, lgs)"
	itext t 0 "goUp _ = Nothing"
 ]

maybeZipper3 :: Page
maybeZipper3 = [\t -> do
	writeTopTitle t "失敗する可能性"
	text t "", \t -> do
	text t "* 関数をつないでいた(-:)を(>>=)に変えれば良い", \t -> do
	itext t 1 "testZipper >>= goRight >>= goLeft"
	itext t 2 ">>= return . modify f"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* zipperについて見てきた", \t -> do
	text t "* 基本的な考えかたは以下の通り", \t -> do
	itext t 1 "- 「全体の構造」と「今いる場所」を", \t -> do
	itext t 1 "- 「残りの構造」と「履歴」で表現する", \t -> do
	text t "* 進むときは進んだことで失われる情報を「履歴」へ追加", \t -> do
	text t "* 戻るには「履歴」と「残りの構造」からもとの構造を復元", \t -> do
	text t "* 使いたい値、変更したい値を表面に出しておくということ"
 ]
