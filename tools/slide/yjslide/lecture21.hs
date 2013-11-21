import Lecture

subtitle :: String
subtitle = "第21回 例外処理"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 例外処理について学ぶ", \t -> do
	text t "* 入出力には例外がつきものである", \t -> do
	text t "* 例外として補足可能なbottom値もある", \t -> do
	itext t 1 "- errorやundefined", \t -> do
	itext t 1 "- IO内に入れる必要がある", \t -> do
	itext t 1 "- ただし遅延評価ゆえの落とし穴に注意が必要", \t -> do
	text t "* 遅延IOにおける例外処理には注意が必要になる", \t -> do
	itext t 1 "- 遅延IOについてはここでは軽く触れるのみ", \t -> do
	itext t 1 "- 遅延IOにおける例外処理については上級編で"
 ]
