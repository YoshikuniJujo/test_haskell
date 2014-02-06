import Lecture

subtitle :: String
subtitle = "第23回 まとめ:オセロ(ウィンドウの処理)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ゲームの表現とその操作を作りAIも作ったので", \t -> do
	text t "* ウィンドウシステムとのやりとりを作ればオセロが完成する", \t -> do
	text t "* 描画関数と入力処理関数とを作る", \t -> do
	text t "* AIの動作はタイマー制御とする", \t -> do
	text t "* ユーザーの入力でゲームが開始し", \t -> do
	text t "* 次がどちらのターンかチェックし", \t -> do
	itext t 1 "- ユーザーなら入力を有効にしタイマーを無効にする", \t -> do
	itext t 1 "- AIなら入力を無効にしタイマーを有効にする"
 ]
