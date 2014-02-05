import Lecture

subtitle :: String
subtitle = "第21回 まとめ:オセロ(ゲームの定義)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, beginGameModule
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回のBoard型には足りないところがある", \t -> do
	text t "* 今がどちらのターンなのかという情報がない", \t -> do
	text t "* Game型はターンの情報とBoard型を持つことにする"
 ]

beginGameModule :: Page
beginGameModule = [\t -> do
	writeTopTitle t "Gameモジュール"
	text t "", \t -> do
	text t "* Game.hsを作り以下を書き込もう", \t -> do
	itext t 1 "module Game (", \t -> do
	itext t 1 ") where"
 ]
