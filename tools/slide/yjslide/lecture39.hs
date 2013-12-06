import Lecture

subtitle :: String
subtitle = "第39回 FlexibleInstances拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* FlexibleInstancesという言語拡張がある"
 ]
