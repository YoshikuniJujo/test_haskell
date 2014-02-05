import Lecture

subtitle :: String
subtitle = "第22回 まとめ:オセロ(AI)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* オセロの例を作ってきた", \t -> do
	text t "* 今回はAIの部分を作る", \t -> do
	text t "* マスごとにポイントをふる", \t -> do
	text t "* 何手か先を読み最善手を選ぶ", \t -> do
	text t "* 相手も同じアルゴリズムを採用すると仮定する", \t -> do
	text t "* それなりに強い、と思う", \t -> do
	text t "* 演者よりは強い"
 ]
