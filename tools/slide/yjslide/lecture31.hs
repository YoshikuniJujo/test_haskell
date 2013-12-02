import Lecture

subtitle :: String
subtitle = "第31回 重み平衡木"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	action, rotation, balance, doubleRotation
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

balance :: Page
balance = [\t -> do
	writeTopTitle t "平衡の維持"
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
