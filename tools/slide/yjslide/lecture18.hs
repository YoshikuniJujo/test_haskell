import Lecture

subtitle :: String
subtitle = "第18回 差分リスト"

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
	text t "* 差分リストについて学ぶ"
 ]
