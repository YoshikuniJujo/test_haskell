import Lecture

subtitle :: String
subtitle = "第20回 総合演習:オセロ(盤とゲームの定義)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, aboutModules, aboutBoard
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellの基本をだいたい学んだ", \t -> do
	text t "* 最後にオセロを作って終わりにしよう", \t -> do
	text t "* 難しい部分があるかもしれないが", \t -> do
	itext t 1 "- 全体の雰囲気がつかめれば良いとする", \t -> do
	text t "* 3回に分けて作っていこう", \t -> do
	text t "* 今回は盤とゲームを定義していこう"
 ]

aboutModules :: Page
aboutModules = [\t -> do
	writeTopTitle t "モジュール"
	text t "", \t -> do
	text t "* 以下のようなモジュールに分ける", \t -> do
	itext t 1 "Main, Window, AI, Game, Board, Tools", \t -> do
	text t "* MainはWindowの関数を呼び出すだけ", \t -> do
	text t "* WindowはAIとGameの関数をGUI上で結合する", \t -> do
	text t "* AIは与えられたゲームの状態からコンピュータの手を計算", \t -> do
	text t "* Gameはゲームの状態を表現する型と操作する関数を定義", \t -> do
	text t "* Boardは盤の状態を表現する型と操作する関数を定義", \t -> do
	text t "* Toolsではより一般的に使えそうな関数を定義する", \t -> do
	text t "* 今回はBoard, GameとToolsの一部を作ろう"
 ]

aboutBoard :: Page
aboutBoard = [\t -> do
	writeTopTitle t "Board"
	text t "", \t -> do
	text t "* lectures/othelloディレクトリを作成しよう", \t -> do
	text t "* Board.hsを作っていこう"
 ]
