import Lecture

subtitle :: String
subtitle = "第21回 型の階層"

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
	text t "* Haskellで型の階層を作る方法を見ていこう"
 ]
