import Lecture

subtitle :: String
subtitle = "第31回 Map型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* キーと値を組にする辞書という構造がある", \t -> do
	text t "* 最も素朴なアルゴリズムは以下の通り", \t -> do
	itext t 1 "- (キー, 値)ペアのリストを作り", \t -> do
	itext t 1 "- キーをひとつずつ見ていき探したいキーを見つける", \t -> do
	itext t 1 "- それとペアになっている値を得る", \t -> do
	text t "* これにはO(n)時間がかかる", \t -> do
	text t "* より効率の良い方法としてハッシュテーブルがある", \t -> do
	text t "* ハッシュテーブルと競合可能なより関数型的なMap型がある"
 ]
