import Lecture

subtitle :: String
subtitle = "第31回 重み平衡木"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	action, rotation, badRotation, doubleRotation, condRot,
	actionDetail,
	mkWBT, mkWBT2, mkWBT3, mkWBT4, mkWBT5, mkWBT6, mkWBT7, mkWBT8,
	mkWBT9, mkWBT10,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 木構造は様々なアルゴリズムで使われる", \t -> do
	text t "* リストだと要素へのアクセスにO(n)時間かかる", \t -> do
	text t "* 木構造ならO(log n)時間ですむ", \t -> do
	text t "* O(log n)時間ですむのは木が平衡しているとき", \t -> do
	text t "* 木の平衡を保つのにはいくつかのアルゴリズムがある", \t -> do
	itext t 1 "- AVL木", \t -> do
	itext t 1 "- 赤黒木", \t -> do
	itext t 1 "- 重み平衡木", \t -> do
	text t "* このうちの重み平衡木について見ていこう"
 ]

action :: Page
action = [\t -> do
	writeTopTitle t "平衡の保ちかた"
	text t "", \t -> do
	text t "* 要素の挿入や削除の後に平衡を保つために木の回転を行う", \t -> do
	itext t 1 "- 挿入や削除のたびに木の平衡をチェックする", \t -> do
	itext t 1 "- 左右の要素数の比がある程度以上になったとき", \t -> do
	itext t 1 "- 木の回転を行う", \t -> do
	itext t 1 "- 条件によっては2重の回転が必要なときもある"
 ]

rotation :: Page
rotation = [\t -> do
	writeTopTitle t "木の回転"
	text t "", \t -> do
	text t "* 左回転の例", \t -> do
	writeTree t (: "") 15 4 100 120 rotLTree0
	rtGoto t 230 170
	rightArrow t
	writeTree t (: "") 15 4 350 120 $ rotateL rotLTree0
	rtGoto t (width / 8) 250, \t -> do
	text t "* 右にあった節(d)が根となり", \t -> do
	text t "* その左下だった節(c)が、もとの根(b)の右下に移る", \t -> do
	text t "* 右回転はこの逆になる"
 ]

badRotation :: Page
badRotation = [\t -> do
	writeTopTitle t "回転後も平衡とならない"
	text t "", \t -> do
	text t "* 「平衡」を「左右の木の大きさの比が一定以内」とする", \t -> do
	writeTree t (: "") 15 4 100 120 rotLTree0
	rtGoto t 230 170
	rightArrow t
	writeTree t (: "") 15 4 350 120 $ rotateL rotLTree0
	rtGoto t (width / 8) 250, \t -> do
	text t "* cの位置にある木が大きかった場合", \t -> do
	itext t 1 "- 回転前後のどちらも平衡とならないことがあり得る", \t -> do
	text t "* このような場合はdを右回転したうえで全体を左回転する", \t -> do
	text t "* これを二重回転と呼ぶ"
 ]

doubleRotation :: Page
doubleRotation = [\t -> do
	writeTopTitle t "二重回転"
	text t "", \t -> do
	text t "* 二重回転の例"
	writeTree t (: "") 15 2 100 120 dRotLTree0
	rtGoto t 160 170
	rightArrow t
	writeTree t (: "") 15 2 240 120 $ mapR rotateR dRotLTree0
	rtGoto t 300 170
	rightArrow t
	writeTree t (: "") 15 2 390 120 $ rotateL $ mapR rotateR dRotLTree0
	rtGoto t (width / 8) 320, \t -> do
	text t "* fを右回転したあとに全体を左回転している"
 ]

condRot :: Page
condRot = [\t -> do
	writeTopTitle t "回転する条件"
	text t "", \t -> do
	writeTree t (: "") 15 4 200 80 rotLTree0
	rtGoto t (width / 8) 210, \t -> do
	text t "* 回転を行うかはaとdの木の大きさの比(delta)による", \t -> do
	text t "* 二重回転を行うかはcとeの木の大きさの比(ratio)による", \t -> do
	text t "* それらの比の取りかたが以下を決める", \t -> do
	itext t 1 "- アルゴリズムとしての正しさ", \t -> do
	itext t 1 "- 効率", \t -> do
	text t "* 紆余曲折があり(3, 2)と(4, 2)のみが正しいことに"
 ]

rotLTree0 :: BinTree Char
rotLTree0 = Bin 'b'
	(Bin 'a' Empty Empty)
	(Bin 'd'
		(Bin 'c' Empty Empty)
		(Bin 'e' Empty Empty))

dRotLTree0 :: BinTree Char
dRotLTree0 = Bin 'b'
	(Bin 'a' Empty Empty)
	(Bin 'f'
		(Bin 'd'
			(Bin 'c' Empty Empty)
			(Bin 'e' Empty Empty))
		(Bin 'g' Empty Empty))

actionDetail :: Page
actionDetail = [\t -> do
	writeTopTitle t "回転する条件"
	text t "", \t -> do
	text t "* HaskellのSet, Mapで使われているのは(3, 2)", \t -> do
	text t "* これは以下を意味する", \t -> do
	itext t 1 "- 左右の木の大きさの比が3を「超えた」ら回転する", \t -> do
	itext t 1 "- 大きいほうの右(左)の木を見て", \t -> do
	itext t 1 "- 左(右)の木のほうが2倍「以上」大きければ二重回転"
 ]

mkWBT :: Page
mkWBT = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* 実際にHaskellで実装していこう", \t -> do
	text t "* まずは型を定義する", \t -> do
	itext t 1 "data WBT a = Empty | Bin {", \t -> do
	itext t 2 "_weight :: Int,"
	itext t 2 "value :: a,"
	itext t 2 "left :: WBT a"
	itext t 2 "right :: WBT a }", \t -> do
	text t "* Emptyにも対応できるように", \t -> do
	itext t 1 "weight :: WBT a -> Int"
	itext t 1 "weight Empty = 0"
	itext t 1 "weight t = _weight t"
 ]

mkWBT2 :: Page
mkWBT2 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* 重さを自動計算してくれる型構築子を作る", \t -> do
	itext t 1 "bin :: a -> WBT a -> WBT a -> WBT a"
	itext t 1 "bin x t1 t2 ="
	itext t 2 "Bin (weight t1 + weight t2 + 1) x t1 t2", \t -> do
	text t "* deltaとratioを定義しておく", \t -> do
	itext t 1 "delta, ratio :: Int"
	itext t 1 "delta = 3"
	itext t 1 "ratio = 2"
 ]

mkWBT3 :: Page
mkWBT3 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* 回転関数を作る", \t -> do
	itext t 1 "rotateL, rotateR :: WBT a -> WBT a"
	itext t 1 "rotateL (Bin _ x lx (Bin _ y ly ry)) ="
	itext t 2 "bin y (bin x lx ly) ry", \t -> do
	itext t 1 "rotateR (Bin _ x (Bin _ y ly ry) rx) ="
	itext t 2 "bin y ly (bin x ry rx)"
 ]

mkWBT4 :: Page
mkWBT4 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* 平衡関数を作る", \t -> do
	itext t 0 "balance :: WBT a -> WBT a"
	itext t 0 "balance Empty = Empty"
	itext t 1 "(続く)"
 ]

mkWBT5 :: Page
mkWBT5 = [\t -> do
	writeTopTitle t "重み平衡木を実装する", \t -> do
	itext t 0 "balance t@(Bin _ x l r)"
	itext t 0.5 "| weight l + weight r <= 1 = t"
	itext t 0.5 "| weight r > delta * weight l ="
	itext t 1 "if weight (left r) >= ratio * weight (right r)"
	itext t 1.5 "then rotateL $ bin x l (rotateR r)"
	itext t 1.5 "else rotateL t"
	itext t 0.5 "| weight l > delta * weight r ="
	itext t 1 "if weight (right l) >= ratio * weight (left l)"
	itext t 1.5 "then rotateR $ bin x (rotateL l) r"
	itext t 1.5 "else rotateR t"
	itext t 0.5 "| otherwise = t"
 ]

mkWBT6 :: Page
mkWBT6 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* 挿入関数を作る", \t -> do
	itext t 1 "insert :: Ord a => a -> WBT a -> WBT a"
	itext t 1 "insert x Empty = bin x Empty Empty"
	itext t 1 "insert x t@(Bin _ x0 l r)"
	itext t 2 "| x < x0 = balance $ bin x0 (insert x l) r"
	itext t 2 "| x > x0 = balance $ bin x0 r (insert x r)"
	itext t 2 "| otherwise = t"
 ]

mkWBT7 :: Page
mkWBT7 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* 削除関数を作る", \t -> do
	itext t 1 "delete :: Ord a => a -> WBT a -> WBT a"
	itext t 1 "delete _ Empty = Empty"
	itext t 1 "delete x (Bin _ x0 l r)"
	itext t 2 "| x < x0 = balance $ bin x0 (delete x l) r"
	itext t 2 "| x > x0 = balance $ bin x0 l (delete x r)"
	itext t 2 "| otherwise = glue l r", \t -> do
	text t "* glueは2つの木をひとつにまとめる"
 ]

mkWBT8 :: Page
mkWBT8 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* glueの定義", \t -> do
	itext t 1 "glue :: WBT a -> WBT a -> WBT a"
	itext t 1 "glue Empty r = r"
	itext t 1 "glue l Empty = l"
	itext t 1 "glue l r"
	itext t 2 "| weight l > weight r ="
	itext t 3 "let (m, l') = deleteFindMax l in"
	itext t 4 "balance $ bin m l' r"
	itext t 2 "| otherwise ="
	itext t 3 "let (m, r') = deleteFindMin r in"
	itext t 4 "balance $ bin m l r'"
 ]

mkWBT9 :: Page
mkWBT9 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* deleteFindMinは最小値とそれを削除した木を返す", \t -> do
	itext t 1 "deleteFindMin :: WBT a -> (a, WBT a)"
	itext t 1 "deleteFindMin (Bin _ x Empty r) = (x, r)"
	itext t 1 "deleteFindMin (Bin _ x l r) = let"
	itext t 2 "(xm, l') deleteFindMin l in"
	itext t 2 "(xm, balance $ bin x l' r)"
 ]

mkWBT10 :: Page
mkWBT10 = [\t -> do
	writeTopTitle t "重み平衡木を実装する"
	text t "", \t -> do
	text t "* deleteFindMaxは最大値とそれを削除した木を返す", \t -> do
	itext t 1 "deleteFindMax :: WBT a -> (a, WBT a)"
	itext t 1 "deleteFindMax (Bin _ x l Empty) = (x, l)"
	itext t 1 "deleteFindMax (Bin _ x l r) = let"
	itext t 2 "(xm, r') = deleteFindMax r in"
	itext t 2 "(xm, balance $ bin x l r')"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 重み平衡木の仕組みを学んだ", \t -> do
	text t "* 実際にHaskellで定義してみた", \t -> do
	text t "* 要素の挿入、削除時に平衡関数が再帰的に実行される", \t -> do
	text t "* 木を作る際に平衡を保つ処理をしておくことで", \t -> do
	itext t 1 "- 値の検索がO(log n)時間でできるようになる", \t -> do
	text t "* それぞれの効率は以下のようになる", \t -> do
	itext t 1 "挿入: O(log n)"
	itext t 1 "削除: O(log n)"
	itext t 1 "検索: O(log n)"
 ]
