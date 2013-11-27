import Lecture

subtitle :: String
subtitle = "第28回 パターンマッチと正格性"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	refutable, irrefutable,
	refutablePatterns, irrefutablePatterns,
	powers, powers2, powers3, powers4, lazyPatternSummary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* パターンマッチ時の値の評価", \t -> do
	itext t 1 "- 評価が行われる場合と行われない場合とがある", \t -> do
	itext t 1 "- 評価が行われるかどうかはいろいろな状況による", \t -> do
	text t "* 遅延パターンやバンパターンを使うことで", \t -> do
	itext t 1 "- デフォルトの動作を変えることができる", \t -> do
	text t "* 以下について見ていこう", \t -> do
	itext t 1 "- どのようなときに値の評価が行われるか", \t -> do
	itext t 1 "- 遅延パターンを使うことでそれがどう変わるか", \t -> do
	itext t 1 "- バンパターンを使うことでそれがどう変わるか"
 ]

refutable :: Page
refutable = [\t -> do
	writeTopTitle t "可反駁パターン"
	text t "", \t -> do
	text t "* パターン照合するときに値の評価をする", \t -> do
	itext t 1 "- これを可反駁パターンと呼ぶ", \t -> do
	text t "* パターン照合が失敗する可能性がある", \t -> do
	text t "* パターン照合の結果には以下の3つの可能性がある", \t -> do
	itext t 1 "成功: 値がパターンに適合する", \t -> do
	itext t 1 "失敗: 値がパターンに適合しない", \t -> do
	itext t 1 "発散: 照合中にエラーが生じる"
 ]

irrefutable :: Page
irrefutable = [\t -> do
	writeTopTitle t "不可反駁パターン"
	text t "", \t -> do
	text t "* パターン照合するときに値の評価をしない", \t -> do
	itext t 1 "- これを不可反駁パターンと呼ぶ", \t -> do
	itext t 1 "- 照合は必ず成功する", \t -> do
	text t "* 照合で得た値を使おうとしたとき", \t -> do
	itext t 1 "- もとの値がパターンに適合しないとき", \t -> do
	arrowIText t 1 "エラーが生じる"
 ]

refutablePatterns :: Page
refutablePatterns = [\t -> do
	writeTopTitle t "何が可反駁パターンか?"
	text t "", \t -> do
	text t "* 型構築子が使われているパターン", \t -> do
	itext t 1 "例: x : xs, Just x, Nothing", \t -> do
	text t "* それが関数の仮引数部やcaseで使われているとき", \t -> do
	text t "* このような場合、照合は失敗する可能性がある"
 ]

irrefutablePatterns :: Page
irrefutablePatterns = [\t -> do
	writeTopTitle t "何が不可反駁パターンか?"
	text t "", \t -> do
	text t "* 型構築子が使われていないパターン", \t -> do
	itext t 1 "例: x, _(ワイルドカード)", \t -> do
	text t "* 上記以外でもパターン束縛のなかで使われている場合", \t -> do
	itext t 1 "例: x : xs = lst, Just x <- someIO", \t -> do
	text t "* このような場合、照合は必ず成功する"
 ]

powers :: Page
powers = [\t -> do
	writeTopTitle t "2の累乗を求める"
	text t "", \t -> do
	text t "* 2の累乗のリストを求めたいと思ったとする", \t -> do
	text t "* いいやりかたを考えた!", \t -> do
	text t "* 2の累乗のリストの差分リストは2の累乗のリストに等しい", \t -> do
	itext t 1 "powers2 :: [Integer]"
	itext t 1 "powers2 = rediff 1 powers2", \t -> do
	text t "* 初期値と差分リストを与えるともとのリストを返す関数", \t -> do
	itext t 1 "rediff :: Integer -> [Integer] -> [Integer]"
	itext t 1 "rediff x0 (d : ds) = x0 : rediff (x0 + d) ds", \t -> do
	text t "* しかし、以下は値が返らない", \t -> do
	itext t 1 "> take 10 powers2"
 ]

powers2 :: Page
powers2 = [\t -> do
	writeTopTitle t "2の累乗を求める"
	text t "", \t -> do
	text t "* 頭のなかで考えてみると問題なく動きそうだ", \t -> do
	itext t 1 "rediff x0 (d : ds) = x0 : rediff (x0 + d) ds", \t -> do
	text t "* powers2の1番目の要素は1であることは明らか", \t -> do
	itext t 1 "- よってrediff 1 powers2はrediff 1 [1, ...]となる", \t -> do
	itext t 1 "- 変形すると1 : rediff (1 + 1) [(2番目), ...]", \t -> do
	itext t 1 "- よって2番目の要素は2となる", \t -> do
	itext t 1 "- 2番目の要素が2なので1 : rediff 2 [2, ...]", \t -> do
	itext t 1 "- 変形すると1 : 2 : rediff (2 + 2) [(3番目), ...]", \t -> do
	itext t 1 "- 3番目の要素は4となる", \t -> do
	text t "* powers2のn番目の要素はn - 1番目の要素のみから決まる", \t -> do
	text t "* 原理的には問題ないはずだ"
 ]

powers3 :: Page
powers3 = [\t -> do
	writeTopTitle t "2の累乗を求める"
	text t "", \t -> do
	text t "* しかし実際には動かない", \t -> do
	text t "* 理由は?", \t -> do
	itext t 1 "- rediffの第二引数の評価が早すぎる", \t -> do
	itext t 1 "- rediff 1 powers2の1番目の要素は明らかに1である", \t -> do
	itext t 1 "- しかし結果を計算する前に"
	itext t 2 "rediffの第二引数を(d : ds)に照合する必要が", \t -> do
	itext t 1 "- (d : ds)に照合するためにrediff 1 powers2を評価", \t -> do
	itext t 1 "- 循環してしまう", \t -> do
	text t "* 解決するには?", \t -> do
	itext t 1 "- 照合を変数が使われるところまで遅らせる"
 ]

powers4 :: Page
powers4 = [\t -> do
	writeTopTitle t "2の累乗を求める"
	text t "", \t -> do
	text t "* 遅延パターンを使う", \t -> do
	itext t 1 "- ~patという形", \t -> do
	text t "* rediffを書き換えてみよう", \t -> do
	itext t 1 "rediff x0 ~(d : ds) = x0 : rediff (x0 + d) ds", \t -> do
	text t "* これを使えば良い", \t -> do
	itext t 1 "powers2 = rediff 1 powers2"
	itext t 1 "> take 10 powers2"
	itext t 1 "[1,2,4,8,16,32,64,128,256,512]"
 ]

lazyPatternSummary :: Page
lazyPatternSummary = [\t -> do
	writeTopTitle t "遅延パターン(まとめ)"
	text t "", \t -> do
	text t "* ~patという形で書くと遅延パターンになる", \t -> do
	text t "* 不可反駁パターンの名の通りこの照合は必ず成功する", \t -> do
	text t "* 値の評価はパターン内の変数の使用時まで遅らされる", \t -> do
	text t "* パターン束縛はデフォルトで遅延パターン"
 ]
