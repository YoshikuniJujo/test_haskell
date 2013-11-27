import Lecture

subtitle :: String
subtitle = "第28回 パターンマッチと正格性"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	refutable, irrefutable,
	refutablePatterns, irrefutablePatterns
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
