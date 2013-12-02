import Lecture

subtitle :: String
subtitle = "第33回 効率的な文字列"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでは文字列は文字のリストである", \t -> do
	text t "* 多くの場合、これで十分である", \t -> do
	text t "* 文字列を一度だけ読み込み一度だけ出力するような場合", \t -> do
	itext t 1 "- 文字列は生成され使われGCされていく", \t -> do
	itext t 1 "- GCのぶんの効率の低下はあるが", \t -> do
	itext t 1 "- 大きな問題はない", \t -> do
	text t "* しかし、文字列は前から順に一度だけ読まれるだけか?", \t -> do
	text t "* 2回以上読まれたり、ランダムアクセスされたり、する", \t -> do
	text t "* このような場合、著しい", \t -> do
	itext t 1 "- 空間効率の低下や", \t -> do
	itext t 1 "- 時間効率の低下が生じる"
 ]
