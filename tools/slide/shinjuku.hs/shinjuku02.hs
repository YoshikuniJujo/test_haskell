import Lecture

subtitle :: String
subtitle = "2. 型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle]
 ]
