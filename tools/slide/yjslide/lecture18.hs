import Lecture

subtitle :: String
subtitle = "第18回 存在型"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]
