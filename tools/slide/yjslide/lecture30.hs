import Lecture

subtitle :: String
subtitle = "第30回 zipper"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
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
